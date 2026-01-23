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

type EnumVariantInfo = (String, u32, Option<crate::parser::ast::EnumVariantPayload>);
type EnumTypesMap = HashMap<String, Vec<EnumVariantInfo>>;

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
    enum_types: EnumTypesMap, // enum name -> [(variant_name, tag, payload_types)]
    functions: HashMap<String, LLVMValueRef>,
    current_function: Option<LLVMValueRef>,
    loop_exit_blocks: Vec<LLVMBasicBlockRef>,
    loop_continue_blocks: Vec<LLVMBasicBlockRef>,
    file_path: PathBuf,
    debug_bounds_checks: bool, // Enable bounds checking when KRAKEN_DEBUG_BOUNDS=1
}

impl LLVMCodegen {
    /// Extract a simple name from a pattern (for parameter names)
    fn extract_pattern_name(pattern: &Pattern) -> Option<String> {
        match pattern {
            Pattern::Identifier(name) => Some(name.clone()),
            Pattern::Tuple { patterns } => patterns.iter().find_map(Self::extract_pattern_name),
            Pattern::Struct { fields, .. } => fields.first().map(|(name, _)| name.clone()),
            _ => None,
        }
    }

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
                    generic_params: _,
                    where_constraints: _,
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
                generic_params: _,
                where_constraints: _,
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
                generic_params: _,
                where_constraints: _,
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
                // Register enum variants with their tag values and payload types
                let variants_with_tags: Vec<(String, u32, Option<EnumVariantPayload>)> = variants
                    .iter()
                    .enumerate()
                    .map(|(i, (variant_name, payload))| {
                        (variant_name.clone(), i as u32, payload.clone())
                    })
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
                pattern,
                type_annotation,
                initializer,
                is_mutable: _,
            } => {
                // Handle tuple destructuring
                if let Pattern::Tuple { patterns } = pattern {
                    unsafe {
                        if let Some(init_expr) = initializer {
                            // Generate the tuple value
                            let tuple_val = self.codegen_expression(init_expr)?;

                            // Extract each element and bind to variables
                            for (index, pat) in patterns.iter().enumerate() {
                                if let Pattern::Identifier(name) = pat {
                                    // Extract element from tuple using extractvalue
                                    let elem_val = LLVMBuildExtractValue(
                                        self.builder,
                                        tuple_val,
                                        index as u32,
                                        c"tuple_elem".as_ptr(),
                                    );

                                    // Create alloca for the variable
                                    let elem_type = LLVMTypeOf(elem_val);
                                    let alloca = self.create_entry_block_alloca(elem_type, name)?;

                                    // Store the extracted value
                                    LLVMBuildStore(self.builder, elem_val, alloca);

                                    // Track the variable
                                    self.named_values.insert(name.clone(), alloca);
                                }
                            }
                        }
                    }
                    return Ok(());
                }

                // Handle simple identifier patterns
                let name = match pattern {
                    Pattern::Identifier(n) => n.clone(),
                    _ => {
                        // Other patterns not yet supported
                        return Ok(());
                    }
                };
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
                    let alloca = self.create_entry_block_alloca(var_type, &name)?;

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

            Statement::ForIn {
                variable,
                iterable,
                body,
            } => {
                unsafe {
                    let function = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("No current function for for-in loop")
                    })?;

                    // Extract range bounds
                    if let Expression::Range {
                        start,
                        end,
                        inclusive,
                    } = iterable
                    {
                        // Allocate loop variable
                        let var_type = LLVMInt64TypeInContext(self.context);
                        let var_alloca = self.create_entry_block_alloca(var_type, variable)?;

                        // Initialize loop variable to start value
                        let start_val = self.codegen_expression(start)?;
                        LLVMBuildStore(self.builder, start_val, var_alloca);
                        self.named_values.insert(variable.clone(), var_alloca);

                        // Evaluate end value once
                        let end_val = self.codegen_expression(end)?;
                        let end_alloca = self.create_entry_block_alloca(var_type, "__range_end")?;
                        LLVMBuildStore(self.builder, end_val, end_alloca);

                        // Create blocks
                        let cond_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            c"for_in.cond".as_ptr(),
                        );
                        let loop_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            c"for_in.body".as_ptr(),
                        );
                        let inc_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            c"for_in.inc".as_ptr(),
                        );
                        let after_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            c"for_in.end".as_ptr(),
                        );

                        // Push loop blocks for break/continue
                        self.loop_exit_blocks.push(after_bb);
                        self.loop_continue_blocks.push(inc_bb);

                        // Branch to condition
                        LLVMBuildBr(self.builder, cond_bb);

                        // Generate condition block: i < end or i <= end
                        LLVMPositionBuilderAtEnd(self.builder, cond_bb);
                        let current_val =
                            LLVMBuildLoad2(self.builder, var_type, var_alloca, c"".as_ptr());
                        let end_loaded =
                            LLVMBuildLoad2(self.builder, var_type, end_alloca, c"".as_ptr());
                        let cond_val = if *inclusive {
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                                current_val,
                                end_loaded,
                                c"".as_ptr(),
                            )
                        } else {
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                                current_val,
                                end_loaded,
                                c"".as_ptr(),
                            )
                        };
                        LLVMBuildCondBr(self.builder, cond_val, loop_bb, after_bb);

                        // Generate loop body
                        LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                        for stmt in &body.statements {
                            self.codegen_statement(stmt)?;
                            let current_bb = LLVMGetInsertBlock(self.builder);
                            if !LLVMGetBasicBlockTerminator(current_bb).is_null() {
                                break;
                            }
                        }
                        let current_bb = LLVMGetInsertBlock(self.builder);
                        if LLVMGetBasicBlockTerminator(current_bb).is_null() {
                            LLVMBuildBr(self.builder, inc_bb);
                        }

                        // Generate increment block: i = i + 1
                        LLVMPositionBuilderAtEnd(self.builder, inc_bb);
                        let current_val =
                            LLVMBuildLoad2(self.builder, var_type, var_alloca, c"".as_ptr());
                        let one = LLVMConstInt(var_type, 1, 0);
                        let next_val = LLVMBuildAdd(self.builder, current_val, one, c"".as_ptr());
                        LLVMBuildStore(self.builder, next_val, var_alloca);
                        LLVMBuildBr(self.builder, cond_bb);

                        // Pop loop blocks
                        self.loop_exit_blocks.pop();
                        self.loop_continue_blocks.pop();

                        // Continue after loop
                        LLVMPositionBuilderAtEnd(self.builder, after_bb);
                    } else {
                        return Err(CompilerError::codegen_error(
                            "For-in loop requires range expression",
                        ));
                    }
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
                            Pattern::Tuple { patterns } => {
                                // Tuple patterns always match - extract elements and jump to arm
                                // Builder is already positioned at the current check block

                                // Extract each element from the tuple and bind to pattern variables
                                for (index, pat) in patterns.iter().enumerate() {
                                    if let Pattern::Identifier(name) = pat {
                                        // Extract element from tuple
                                        let elem_val = LLVMBuildExtractValue(
                                            self.builder,
                                            match_val,
                                            index as u32,
                                            c"match_tuple_elem".as_ptr(),
                                        );

                                        // Create alloca for the variable
                                        let elem_type = LLVMTypeOf(elem_val);
                                        let alloca =
                                            self.create_entry_block_alloca(elem_type, name)?;

                                        // Store the extracted value
                                        LLVMBuildStore(self.builder, elem_val, alloca);

                                        // Track the variable for the arm body
                                        self.named_values.insert(name.clone(), alloca);
                                    }
                                }

                                // Jump to the arm block
                                LLVMBuildBr(self.builder, arm_blocks[i]);
                            }
                            Pattern::EnumVariant {
                                enum_name,
                                variant_name,
                                bindings,
                            } => {
                                // Look up the tag value for this variant
                                if let Some(variants) = self.enum_types.get(enum_name).cloned() {
                                    if let Some((_, tag, _payload_types)) =
                                        variants.iter().find(|(name, _, _)| name == variant_name)
                                    {
                                        // Compare match value (assumed to be tag) against expected tag
                                        let i64_ty = LLVMInt64TypeInContext(self.context);
                                        let expected_tag = LLVMConstInt(i64_ty, *tag as u64, 0);
                                        let cmp_name =
                                            CString::new(format!("enum.cmp.{variant_name}"))
                                                .expect("CString failed");
                                        let cond = LLVMBuildICmp(
                                            self.builder,
                                            LLVMIntPredicate::LLVMIntEQ,
                                            match_val,
                                            expected_tag,
                                            cmp_name.as_ptr(),
                                        );
                                        LLVMBuildCondBr(
                                            self.builder,
                                            cond,
                                            arm_blocks[i],
                                            next_check_blocks[i],
                                        );

                                        // TODO: For payload extraction, we need to:
                                        // 1. Load payload data from the enum struct
                                        // 2. Bind each payload element to the corresponding binding name
                                        // For now, bindings are placeholders (payload support is in progress)
                                        let _ = bindings; // Suppress unused warning
                                    } else {
                                        // Variant not found, just branch (will error at runtime)
                                        LLVMBuildBr(self.builder, arm_blocks[i]);
                                    }
                                } else {
                                    // Enum not found, just branch
                                    LLVMBuildBr(self.builder, arm_blocks[i]);
                                }
                            }
                            Pattern::Range {
                                start,
                                end,
                                inclusive,
                            } => {
                                // Range pattern: check if match_val is within range
                                let start_val = self.codegen_expression(start)?;
                                let end_val = self.codegen_expression(end)?;

                                // Check if match_val >= start
                                let ge_cond = LLVMBuildICmp(
                                    self.builder,
                                    llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                                    match_val,
                                    start_val,
                                    c"range.ge".as_ptr(),
                                );

                                // Check if match_val < end (or <= end if inclusive)
                                let le_cond = if *inclusive {
                                    LLVMBuildICmp(
                                        self.builder,
                                        llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                                        match_val,
                                        end_val,
                                        c"range.le".as_ptr(),
                                    )
                                } else {
                                    LLVMBuildICmp(
                                        self.builder,
                                        llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                                        match_val,
                                        end_val,
                                        c"range.lt".as_ptr(),
                                    )
                                };

                                // Combine conditions with AND
                                let cond = LLVMBuildAnd(
                                    self.builder,
                                    ge_cond,
                                    le_cond,
                                    c"range.cond".as_ptr(),
                                );

                                // Branch to arm or next check
                                LLVMBuildCondBr(
                                    self.builder,
                                    cond,
                                    arm_blocks[i],
                                    next_check_blocks[i],
                                );
                            }
                            Pattern::Or { patterns } => {
                                // Or pattern: check if match_val matches any of the alternatives
                                // Build a chain of OR conditions
                                let mut combined_cond = None;

                                for pat in patterns {
                                    match pat {
                                        Pattern::Literal(lit_expr) => {
                                            let lit_val = self.codegen_expression(lit_expr)?;
                                            let cmp = LLVMBuildICmp(
                                                self.builder,
                                                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                                                match_val,
                                                lit_val,
                                                c"or.cmp".as_ptr(),
                                            );

                                            combined_cond =
                                                Some(if let Some(prev) = combined_cond {
                                                    LLVMBuildOr(
                                                        self.builder,
                                                        prev,
                                                        cmp,
                                                        c"or.combined".as_ptr(),
                                                    )
                                                } else {
                                                    cmp
                                                });
                                        }
                                        _ => {
                                            // Other pattern types in or patterns would need more complex handling
                                            // For now, just accept them (will be validated by type checker)
                                        }
                                    }
                                }

                                if let Some(cond) = combined_cond {
                                    LLVMBuildCondBr(
                                        self.builder,
                                        cond,
                                        arm_blocks[i],
                                        next_check_blocks[i],
                                    );
                                } else {
                                    // No conditions, just branch to arm
                                    LLVMBuildBr(self.builder, arm_blocks[i]);
                                }
                            }
                            Pattern::Struct {
                                struct_name: _,
                                fields: _,
                                partial: _,
                            } => {
                                // Struct pattern: extract fields and bind to variables
                                // For struct patterns, we always match (type checking ensures correctness)
                                // The actual field extraction and binding will happen in the arm body
                                // when variables are accessed. For now, just branch to the arm block.
                                LLVMBuildBr(self.builder, arm_blocks[i]);
                            }
                        }

                        // Check guard clause if present
                        if arm.guard.is_some() {
                            // Guard clauses require additional condition checking
                            // Position at the arm block and add guard check
                            LLVMPositionBuilderAtEnd(self.builder, arm_blocks[i]);

                            if let Some(guard_expr) = &arm.guard {
                                let guard_val = self.codegen_expression(guard_expr)?;

                                // Create blocks for guard success and failure
                                let guard_success = LLVMAppendBasicBlockInContext(
                                    self.context,
                                    self.current_function.unwrap(),
                                    c"guard.success".as_ptr(),
                                );
                                let guard_fail = next_check_blocks[i];

                                // Branch based on guard condition
                                LLVMBuildCondBr(self.builder, guard_val, guard_success, guard_fail);

                                // Position at guard success block for arm body
                                LLVMPositionBuilderAtEnd(self.builder, guard_success);
                                arm_blocks[i] = guard_success; // Update arm block to guard success
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

            // strncat: char* strncat(char* dest, const char* src, int n)
            let strncat_type = LLVMFunctionType(
                i8_ptr_type,
                [i8_ptr_type, i8_ptr_type, int_type].as_mut_ptr(),
                3,
                0,
            );
            let strncat_name = CString::new("strncat").expect("CString failed");
            let strncat_func = LLVMAddFunction(self.module, strncat_name.as_ptr(), strncat_type);
            self.functions.insert("strncat".to_string(), strncat_func);

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

            // Kraken runtime functions
            // kraken_str_split: VecString* kraken_str_split(const char* s, const char* delim)
            let kraken_str_split_type =
                LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let kraken_str_split_name = CString::new("kraken_str_split").expect("CString failed");
            let kraken_str_split_func = LLVMAddFunction(
                self.module,
                kraken_str_split_name.as_ptr(),
                kraken_str_split_type,
            );
            self.functions
                .insert("kraken_str_split".to_string(), kraken_str_split_func);

            // kraken_str_join: char* kraken_str_join(VecString* vec, const char* sep)
            let kraken_str_join_type =
                LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let kraken_str_join_name = CString::new("kraken_str_join").expect("CString failed");
            let kraken_str_join_func = LLVMAddFunction(
                self.module,
                kraken_str_join_name.as_ptr(),
                kraken_str_join_type,
            );
            self.functions
                .insert("kraken_str_join".to_string(), kraken_str_join_func);

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
            // usleep: int usleep(useconds_t usec) - uses i32 on most platforms
            let i32_type = LLVMInt32TypeInContext(self.context);
            let usleep_type = LLVMFunctionType(i32_type, [i32_type].as_mut_ptr(), 1, 0);
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

            // time: time_t time(time_t *tloc)
            let time_type = LLVMFunctionType(int_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let time_name = CString::new("time").expect("CString failed");
            let time_func = LLVMAddFunction(self.module, time_name.as_ptr(), time_type);
            self.functions.insert("time".to_string(), time_func);

            // =============================================================
            // POSIX Threading (pthreads)
            // =============================================================

            // pthread_create: int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
            //                                    void *(*start_routine)(void*), void *arg)
            let pthread_create_type = LLVMFunctionType(
                i32_type,
                [i8_ptr_type, i8_ptr_type, i8_ptr_type, i8_ptr_type].as_mut_ptr(),
                4,
                0,
            );
            let pthread_create_name = CString::new("pthread_create").expect("CString failed");
            let pthread_create_func = LLVMAddFunction(
                self.module,
                pthread_create_name.as_ptr(),
                pthread_create_type,
            );
            self.functions
                .insert("pthread_create".to_string(), pthread_create_func);

            // pthread_join: int pthread_join(pthread_t thread, void **retval)
            let pthread_join_type =
                LLVMFunctionType(i32_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let pthread_join_name = CString::new("pthread_join").expect("CString failed");
            let pthread_join_func =
                LLVMAddFunction(self.module, pthread_join_name.as_ptr(), pthread_join_type);
            self.functions
                .insert("pthread_join".to_string(), pthread_join_func);

            // pthread_detach: int pthread_detach(pthread_t thread)
            let pthread_detach_type = LLVMFunctionType(i32_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let pthread_detach_name = CString::new("pthread_detach").expect("CString failed");
            let pthread_detach_func = LLVMAddFunction(
                self.module,
                pthread_detach_name.as_ptr(),
                pthread_detach_type,
            );
            self.functions
                .insert("pthread_detach".to_string(), pthread_detach_func);

            // pthread_self: pthread_t pthread_self(void)
            let pthread_self_type = LLVMFunctionType(i8_ptr_type, [].as_mut_ptr(), 0, 0);
            let pthread_self_name = CString::new("pthread_self").expect("CString failed");
            let pthread_self_func =
                LLVMAddFunction(self.module, pthread_self_name.as_ptr(), pthread_self_type);
            self.functions
                .insert("pthread_self".to_string(), pthread_self_func);

            // =============================================================
            // POSIX Mutex
            // =============================================================

            // pthread_mutex_init: int pthread_mutex_init(pthread_mutex_t *mutex, const pthread_mutexattr_t *attr)
            let pthread_mutex_init_type =
                LLVMFunctionType(i32_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let pthread_mutex_init_name =
                CString::new("pthread_mutex_init").expect("CString failed");
            let pthread_mutex_init_func = LLVMAddFunction(
                self.module,
                pthread_mutex_init_name.as_ptr(),
                pthread_mutex_init_type,
            );
            self.functions
                .insert("pthread_mutex_init".to_string(), pthread_mutex_init_func);

            // pthread_mutex_lock: int pthread_mutex_lock(pthread_mutex_t *mutex)
            let pthread_mutex_lock_type =
                LLVMFunctionType(i32_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let pthread_mutex_lock_name =
                CString::new("pthread_mutex_lock").expect("CString failed");
            let pthread_mutex_lock_func = LLVMAddFunction(
                self.module,
                pthread_mutex_lock_name.as_ptr(),
                pthread_mutex_lock_type,
            );
            self.functions
                .insert("pthread_mutex_lock".to_string(), pthread_mutex_lock_func);

            // pthread_mutex_unlock: int pthread_mutex_unlock(pthread_mutex_t *mutex)
            let pthread_mutex_unlock_type =
                LLVMFunctionType(i32_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let pthread_mutex_unlock_name =
                CString::new("pthread_mutex_unlock").expect("CString failed");
            let pthread_mutex_unlock_func = LLVMAddFunction(
                self.module,
                pthread_mutex_unlock_name.as_ptr(),
                pthread_mutex_unlock_type,
            );
            self.functions.insert(
                "pthread_mutex_unlock".to_string(),
                pthread_mutex_unlock_func,
            );

            // pthread_mutex_destroy: int pthread_mutex_destroy(pthread_mutex_t *mutex)
            let pthread_mutex_destroy_type =
                LLVMFunctionType(i32_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let pthread_mutex_destroy_name =
                CString::new("pthread_mutex_destroy").expect("CString failed");
            let pthread_mutex_destroy_func = LLVMAddFunction(
                self.module,
                pthread_mutex_destroy_name.as_ptr(),
                pthread_mutex_destroy_type,
            );
            self.functions.insert(
                "pthread_mutex_destroy".to_string(),
                pthread_mutex_destroy_func,
            );

            // =============================================================
            // POSIX Condition Variables
            // =============================================================

            // pthread_cond_init: int pthread_cond_init(pthread_cond_t *cond, const pthread_condattr_t *attr)
            let pthread_cond_init_type =
                LLVMFunctionType(i32_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let pthread_cond_init_name = CString::new("pthread_cond_init").expect("CString failed");
            let pthread_cond_init_func = LLVMAddFunction(
                self.module,
                pthread_cond_init_name.as_ptr(),
                pthread_cond_init_type,
            );
            self.functions
                .insert("pthread_cond_init".to_string(), pthread_cond_init_func);

            // pthread_cond_wait: int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex)
            let pthread_cond_wait_type =
                LLVMFunctionType(i32_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let pthread_cond_wait_name = CString::new("pthread_cond_wait").expect("CString failed");
            let pthread_cond_wait_func = LLVMAddFunction(
                self.module,
                pthread_cond_wait_name.as_ptr(),
                pthread_cond_wait_type,
            );
            self.functions
                .insert("pthread_cond_wait".to_string(), pthread_cond_wait_func);

            // pthread_cond_signal: int pthread_cond_signal(pthread_cond_t *cond)
            let pthread_cond_signal_type =
                LLVMFunctionType(i32_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let pthread_cond_signal_name =
                CString::new("pthread_cond_signal").expect("CString failed");
            let pthread_cond_signal_func = LLVMAddFunction(
                self.module,
                pthread_cond_signal_name.as_ptr(),
                pthread_cond_signal_type,
            );
            self.functions
                .insert("pthread_cond_signal".to_string(), pthread_cond_signal_func);

            // pthread_cond_broadcast: int pthread_cond_broadcast(pthread_cond_t *cond)
            let pthread_cond_broadcast_type =
                LLVMFunctionType(i32_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let pthread_cond_broadcast_name =
                CString::new("pthread_cond_broadcast").expect("CString failed");
            let pthread_cond_broadcast_func = LLVMAddFunction(
                self.module,
                pthread_cond_broadcast_name.as_ptr(),
                pthread_cond_broadcast_type,
            );
            self.functions.insert(
                "pthread_cond_broadcast".to_string(),
                pthread_cond_broadcast_func,
            );

            // pthread_cond_destroy: int pthread_cond_destroy(pthread_cond_t *cond)
            let pthread_cond_destroy_type =
                LLVMFunctionType(i32_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let pthread_cond_destroy_name =
                CString::new("pthread_cond_destroy").expect("CString failed");
            let pthread_cond_destroy_func = LLVMAddFunction(
                self.module,
                pthread_cond_destroy_name.as_ptr(),
                pthread_cond_destroy_type,
            );
            self.functions.insert(
                "pthread_cond_destroy".to_string(),
                pthread_cond_destroy_func,
            );

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

                // Extract parameter name from pattern
                let param_name = Self::extract_pattern_name(&param.pattern)
                    .unwrap_or_else(|| format!("param_{i}"));

                let param_name_cstr = CString::new(param_name.as_str()).expect("CString failed");
                LLVMSetValueName2(param_val, param_name_cstr.as_ptr(), param_name.len());

                if let Type::Custom(struct_name) = &param.param_type {
                    self.struct_variables
                        .insert(param_name.clone(), struct_name.clone());
                }

                // Allocate stack space for parameter
                let param_type = self.get_llvm_type(&param.param_type);
                let alloca = self.create_entry_block_alloca(param_type, &param_name)?;

                // Store parameter value into alloca
                LLVMBuildStore(self.builder, param_val, alloca);

                // Store alloca in named_values
                self.named_values.insert(param_name, alloca);
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

                Expression::Call {
                    callee,
                    type_args: _,
                    arguments,
                } => {
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
                            let malloc_fn = *self
                                .functions
                                .get("malloc")
                                .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
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

                        // NOTE: mutex_lock/mutex_unlock now handled by pthread-based stdlib handler

                        if name == "mutex_free" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "mutex_free expects 1 argument (mutex handle)",
                                ));
                            }
                            let mutex = self.codegen_expression(&arguments[0])?;

                            // Free the allocated memory
                            let free_fn = *self
                                .functions
                                .get("free")
                                .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
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
                            let malloc_fn = *self
                                .functions
                                .get("malloc")
                                .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
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

                        // channel_send, channel_recv, channel_close are handled in codegen_stdlib_call

                        // AtomicInt intrinsics
                        if name == "atomic_new" {
                            // Allocate 8 bytes for an i64 atomic value
                            let malloc_fn = *self
                                .functions
                                .get("malloc")
                                .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
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
                            let load = LLVMBuildLoad2(
                                self.builder,
                                i64_ty,
                                ptr_typed,
                                c"atomic.load".as_ptr(),
                            );
                            LLVMSetOrdering(
                                load,
                                llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingAcquire,
                            );
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
                            LLVMSetOrdering(
                                store,
                                llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingRelease,
                            );
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
                            let success = LLVMBuildExtractValue(
                                self.builder,
                                result,
                                1,
                                c"cas.success".as_ptr(),
                            );
                            // Zero-extend i1 to i64
                            let success_i64 =
                                LLVMBuildZExt(self.builder, success, i64_ty, c"".as_ptr());
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
                            let usleep_fn = *self
                                .functions
                                .get("usleep")
                                .ok_or_else(|| CompilerError::codegen_error("Missing usleep"))?;
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
                            let malloc_fn = *self
                                .functions
                                .get("malloc")
                                .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
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
                                let i8_ptr_ty =
                                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                                return Ok(LLVMConstNull(i8_ptr_ty));
                            }
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        // Executor intrinsics
                        if name == "executor_new" {
                            // Allocate executor struct
                            let malloc_fn = *self
                                .functions
                                .get("malloc")
                                .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
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

                        if name == "executor_spawn"
                            || name == "executor_run"
                            || name == "executor_shutdown"
                        {
                            // Placeholder implementations
                            for arg in arguments {
                                let _ = self.codegen_expression(arg)?;
                            }
                            if name == "executor_spawn" {
                                let i8_ptr_ty =
                                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                                return Ok(LLVMConstNull(i8_ptr_ty));
                            }
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        // Cancellation intrinsics
                        if name == "cancel_token_new" {
                            // Allocate token: single i64 flag (0 = not cancelled, 1 = cancelled)
                            let malloc_fn = *self
                                .functions
                                .get("malloc")
                                .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
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
                            let val = LLVMBuildLoad2(
                                self.builder,
                                i64_ty,
                                ptr_typed,
                                c"cancelled".as_ptr(),
                            );
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
                        let trap_fn = *self
                            .functions
                            .get("abort")
                            .ok_or_else(|| CompilerError::codegen_error("Missing abort"))?;
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
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let memcpy_fn = *self
                        .functions
                        .get("memcpy")
                        .ok_or_else(|| CompilerError::codegen_error("Missing memcpy"))?;
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

                Expression::StructLiteral {
                    name,
                    type_args: _,
                    fields,
                } => {
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
                    // If it's an identifier, check if it's a function or variable
                    if let Expression::Identifier(name) = &**expression {
                        // First check if it's a function - try direct name and mangled names
                        let func_ref = self.functions.get(name).copied().or_else(|| {
                            // Try to find function with module prefix (e.g., __m..._name)
                            self.functions
                                .iter()
                                .find(|(k, _)| k.ends_with(&format!("_{name}")))
                                .map(|(_, &v)| v)
                        });

                        if let Some(func) = func_ref {
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            Ok(LLVMBuildPtrToInt(
                                self.builder,
                                func,
                                i64_ty,
                                c"fn.ptr".as_ptr(),
                            ))
                        } else if let Some(&alloca) = self.named_values.get(name) {
                            // It's a variable - return its alloca pointer
                            Ok(alloca)
                        } else {
                            Err(CompilerError::codegen_error(format!("Undefined: {name}")))
                        }
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

                Expression::EnumVariant {
                    enum_name,
                    variant_name,
                    payload,
                } => {
                    // Get the tag value for this variant
                    if let Some(variants) = self.enum_types.get(enum_name).cloned() {
                        if let Some((_, tag, payload_types)) =
                            variants.iter().find(|(name, _, _)| name == variant_name)
                        {
                            let i64_ty = LLVMInt64TypeInContext(self.context);

                            // Check if this variant has a payload
                            if let (Some(payload_exprs), Some(payload_type)) =
                                (payload, payload_types)
                            {
                                let payload_types_vec = match payload_type {
                                    EnumVariantPayload::Tuple(types) => types.clone(),
                                    EnumVariantPayload::Struct(fields) => {
                                        fields.iter().map(|(_, ty)| ty.clone()).collect()
                                    }
                                };

                                if !payload_exprs.is_empty() && !payload_types_vec.is_empty() {
                                    // Create a tagged union: { tag: i64, payload_0: T0, payload_1: T1, ... }
                                    let mut field_types = vec![i64_ty];
                                    for ptype in &payload_types_vec {
                                        field_types.push(self.get_llvm_type(ptype));
                                    }

                                    let struct_ty = LLVMStructTypeInContext(
                                        self.context,
                                        field_types.as_mut_ptr(),
                                        field_types.len() as u32,
                                        0, // not packed
                                    );

                                    // Allocate the struct
                                    let alloc_name =
                                        CString::new(format!("{enum_name}.{variant_name}"))
                                            .expect("CString failed");
                                    let struct_ptr = LLVMBuildAlloca(
                                        self.builder,
                                        struct_ty,
                                        alloc_name.as_ptr(),
                                    );

                                    // Store the tag
                                    let tag_val = LLVMConstInt(i64_ty, *tag as u64, 0);
                                    let tag_ptr = LLVMBuildStructGEP2(
                                        self.builder,
                                        struct_ty,
                                        struct_ptr,
                                        0,
                                        c"tag.ptr".as_ptr(),
                                    );
                                    LLVMBuildStore(self.builder, tag_val, tag_ptr);

                                    // Store each payload value
                                    for (idx, payload_expr) in payload_exprs.iter().enumerate() {
                                        let payload_val = self.codegen_expression(payload_expr)?;
                                        let payload_ptr = LLVMBuildStructGEP2(
                                            self.builder,
                                            struct_ty,
                                            struct_ptr,
                                            (idx + 1) as u32,
                                            c"payload.ptr".as_ptr(),
                                        );
                                        LLVMBuildStore(self.builder, payload_val, payload_ptr);
                                    }

                                    // Load the tag for pattern matching (simple enums just return tag)
                                    // For full support, we'd return the struct pointer
                                    let loaded_tag = LLVMBuildLoad2(
                                        self.builder,
                                        i64_ty,
                                        tag_ptr,
                                        c"tag".as_ptr(),
                                    );
                                    return Ok(loaded_tag);
                                }
                            }

                            // Simple enum without payload - just return the tag
                            Ok(LLVMConstInt(i64_ty, *tag as u64, 0))
                        } else {
                            Err(CompilerError::codegen_error(format!(
                                "Unknown variant '{variant_name}' for enum '{enum_name}'"
                            )))
                        }
                    } else {
                        Err(CompilerError::codegen_error(format!(
                            "Unknown enum '{enum_name}'"
                        )))
                    }
                }

                Expression::Tuple { elements } => {
                    // Create tuple as LLVM struct
                    let mut field_values = Vec::new();
                    let mut field_types = Vec::new();

                    for elem in elements {
                        let val = self.codegen_expression(elem)?;
                        field_values.push(val);
                        field_types.push(LLVMTypeOf(val));
                    }

                    // Create struct type for tuple
                    let struct_ty = LLVMStructTypeInContext(
                        self.context,
                        field_types.as_mut_ptr(),
                        field_types.len() as u32,
                        0,
                    );

                    // Allocate space for tuple on stack
                    let alloc = LLVMBuildAlloca(self.builder, struct_ty, c"tuple".as_ptr());

                    // Store each field
                    for (i, val) in field_values.iter().enumerate() {
                        let field_ptr = LLVMBuildStructGEP2(
                            self.builder,
                            struct_ty,
                            alloc,
                            i as u32,
                            c"field".as_ptr(),
                        );
                        LLVMBuildStore(self.builder, *val, field_ptr);
                    }

                    // Load the complete tuple value
                    Ok(LLVMBuildLoad2(
                        self.builder,
                        struct_ty,
                        alloc,
                        c"tuple_val".as_ptr(),
                    ))
                }

                Expression::TupleIndex { tuple, index } => {
                    // Get the tuple value
                    let tuple_val = self.codegen_expression(tuple)?;

                    // Extract the field at the given index using extractvalue
                    Ok(LLVMBuildExtractValue(
                        self.builder,
                        tuple_val,
                        *index as u32,
                        c"tuple_elem".as_ptr(),
                    ))
                }

                Expression::Try { expression } => {
                    // Try operator should be desugared before codegen
                    // For now, just codegen the inner expression
                    // TODO: Implement proper desugaring before this stage
                    self.codegen_expression(expression)
                }

                Expression::Closure {
                    parameters: _,
                    return_type: _,
                    body: _,
                    is_move: _,
                } => {
                    // Closures require environment struct generation and heap allocation
                    // This is a complex feature that requires significant LLVM infrastructure
                    // TODO: Implement closure environment generation and calling convention
                    Err(CompilerError::codegen_error(
                        "Closures are not yet fully implemented in LLVM codegen",
                    ))
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
                Type::Tuple { element_types } => {
                    // Tuple is a struct with numbered fields
                    let mut field_types: Vec<LLVMTypeRef> = element_types
                        .iter()
                        .map(|t| self.get_llvm_type(t))
                        .collect();
                    LLVMStructTypeInContext(
                        self.context,
                        field_types.as_mut_ptr(),
                        field_types.len() as u32,
                        0,
                    )
                }
                Type::Function {
                    param_types,
                    return_type,
                } => {
                    // Function type is represented as a function pointer
                    let mut param_llvm_types: Vec<LLVMTypeRef> =
                        param_types.iter().map(|t| self.get_llvm_type(t)).collect();
                    let return_llvm_type = self.get_llvm_type(return_type);
                    let fn_type = LLVMFunctionType(
                        return_llvm_type,
                        param_llvm_types.as_mut_ptr(),
                        param_llvm_types.len() as u32,
                        0,
                    );
                    LLVMPointerType(fn_type, 0)
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

                    // Trap on empty vector
                    let current_bb = LLVMGetInsertBlock(self.builder);
                    let current_fn = LLVMGetBasicBlockParent(current_bb);
                    let trap_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"vec.int.pop.trap".as_ptr(),
                    );
                    let ok_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"vec.int.pop.ok".as_ptr(),
                    );

                    let is_empty = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntEQ,
                        len_val,
                        LLVMConstInt(i64_ty, 0, 0),
                        c"is.empty".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, is_empty, trap_bb, ok_bb);

                    LLVMPositionBuilderAtEnd(self.builder, trap_bb);
                    let abort_fn = *self
                        .functions
                        .get("abort")
                        .ok_or_else(|| CompilerError::codegen_error("Missing abort"))?;
                    let abort_ty = LLVMGlobalGetValueType(abort_fn);
                    LLVMBuildCall2(
                        self.builder,
                        abort_ty,
                        abort_fn,
                        std::ptr::null_mut(),
                        0,
                        c"".as_ptr(),
                    );
                    LLVMBuildUnreachable(self.builder);

                    LLVMPositionBuilderAtEnd(self.builder, ok_bb);

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
                    let old_cap =
                        LLVMBuildLoad2(self.builder, i64_ty, cap_field, c"old_cap".as_ptr());

                    // Check if reallocation needed: new_cap > old_cap
                    let needs_realloc = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                        new_cap,
                        old_cap,
                        c"needs_realloc".as_ptr(),
                    );

                    // Get current function for block creation
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let realloc_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"reserve.realloc".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"reserve.done".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, needs_realloc, realloc_bb, done_bb);

                    // Realloc block
                    LLVMPositionBuilderAtEnd(self.builder, realloc_bb);

                    let realloc_fn = *self
                        .functions
                        .get("realloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing realloc"))?;
                    let realloc_ty = LLVMGlobalGetValueType(realloc_fn);

                    // Get data ptr
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let old_data =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"old_data".as_ptr());
                    let old_data_i8 =
                        LLVMBuildBitCast(self.builder, old_data, i8_ptr_ty, c"".as_ptr());

                    // New size = new_cap * 8
                    let new_size = LLVMBuildMul(
                        self.builder,
                        new_cap,
                        LLVMConstInt(i64_ty, 8, 0),
                        c"new_size".as_ptr(),
                    );

                    // Realloc
                    let new_data = LLVMBuildCall2(
                        self.builder,
                        realloc_ty,
                        realloc_fn,
                        [old_data_i8, new_size].as_mut_ptr(),
                        2,
                        c"new_data".as_ptr(),
                    );
                    let new_data_typed =
                        LLVMBuildBitCast(self.builder, new_data, i64_ptr_ty, c"".as_ptr());

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
                    let len = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"len".as_ptr());

                    // Get capacity
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

                    // Check if shrink needed: cap > len
                    let needs_shrink = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                        cap,
                        len,
                        c"needs_shrink".as_ptr(),
                    );

                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let shrink_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"shrink.do".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"shrink.done".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, needs_shrink, shrink_bb, done_bb);

                    // Shrink block
                    LLVMPositionBuilderAtEnd(self.builder, shrink_bb);

                    let realloc_fn = *self
                        .functions
                        .get("realloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing realloc"))?;
                    let realloc_ty = LLVMGlobalGetValueType(realloc_fn);

                    // Get data ptr
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let old_data =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"old_data".as_ptr());
                    let old_data_i8 =
                        LLVMBuildBitCast(self.builder, old_data, i8_ptr_ty, c"".as_ptr());

                    // New size = len * 8 (minimum 8 bytes to avoid zero alloc)
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let min_len = LLVMBuildSelect(
                        self.builder,
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                            len,
                            one,
                            c"".as_ptr(),
                        ),
                        one,
                        len,
                        c"min_len".as_ptr(),
                    );
                    let new_size = LLVMBuildMul(
                        self.builder,
                        min_len,
                        LLVMConstInt(i64_ty, 8, 0),
                        c"new_size".as_ptr(),
                    );

                    // Realloc
                    let new_data = LLVMBuildCall2(
                        self.builder,
                        realloc_ty,
                        realloc_fn,
                        [old_data_i8, new_size].as_mut_ptr(),
                        2,
                        c"new_data".as_ptr(),
                    );
                    let new_data_typed =
                        LLVMBuildBitCast(self.builder, new_data, i64_ptr_ty, c"".as_ptr());

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
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);

                    // Allocate struct (24 bytes: ptr + len + cap)
                    let struct_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 24, 0)].as_mut_ptr(),
                        1,
                        c"vec".as_ptr(),
                    );

                    // Allocate data array (capacity * 8 bytes for i64)
                    let data_size = LLVMBuildMul(
                        self.builder,
                        capacity,
                        LLVMConstInt(i64_ty, 8, 0),
                        c"size".as_ptr(),
                    );
                    let array_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [data_size].as_mut_ptr(),
                        1,
                        c"data".as_ptr(),
                    );
                    let array_typed =
                        LLVMBuildBitCast(self.builder, array_ptr, i64_ptr_ty, c"".as_ptr());

                    // Store ptr
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        struct_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, array_typed, ptr_field);

                    // Store len = 0
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

                    // Store cap = capacity
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
                    LLVMBuildStore(self.builder, capacity, cap_field);

                    Ok(Some(struct_ptr))
                }
                "vec_int_swap_remove" => {
                    // O(1) remove: swap element at index with last element, then pop
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;

                    // Get data ptr
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"data".as_ptr());

                    // Get len
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
                    let len = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"len".as_ptr());

                    // Get element at index (to return)
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [index].as_mut_ptr(),
                        1,
                        c"elem".as_ptr(),
                    );
                    let removed_val =
                        LLVMBuildLoad2(self.builder, i64_ty, elem_ptr, c"removed".as_ptr());

                    // Get last index
                    let last_idx = LLVMBuildSub(
                        self.builder,
                        len,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"last".as_ptr(),
                    );

                    // Get last element
                    let last_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [last_idx].as_mut_ptr(),
                        1,
                        c"last_elem".as_ptr(),
                    );
                    let last_val =
                        LLVMBuildLoad2(self.builder, i64_ty, last_ptr, c"last_val".as_ptr());

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
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"data".as_ptr());

                    // Get len
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
                    let len = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"len".as_ptr());

                    // Calculate bytes to move: (len - index) * 8
                    let elements_to_move =
                        LLVMBuildSub(self.builder, len, index, c"tomove".as_ptr());
                    let bytes_to_move = LLVMBuildMul(
                        self.builder,
                        elements_to_move,
                        LLVMConstInt(i64_ty, 8, 0),
                        c"bytes".as_ptr(),
                    );

                    // Source: data[index], Dest: data[index+1]
                    let src_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [index].as_mut_ptr(),
                        1,
                        c"src".as_ptr(),
                    );
                    let index_plus_one = LLVMBuildAdd(
                        self.builder,
                        index,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"idx1".as_ptr(),
                    );
                    let dst_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [index_plus_one].as_mut_ptr(),
                        1,
                        c"dst".as_ptr(),
                    );

                    // memmove(dst, src, bytes)
                    let memmove_fn = *self
                        .functions
                        .get("memmove")
                        .ok_or_else(|| CompilerError::codegen_error("Missing memmove"))?;
                    let memmove_ty = LLVMGlobalGetValueType(memmove_fn);
                    let src_i8 = LLVMBuildBitCast(self.builder, src_ptr, i8_ptr_ty, c"".as_ptr());
                    let dst_i8 = LLVMBuildBitCast(self.builder, dst_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        memmove_ty,
                        memmove_fn,
                        [dst_i8, src_i8, bytes_to_move].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    // Store value at index
                    LLVMBuildStore(self.builder, value, src_ptr);

                    // Increment len
                    let new_len = LLVMBuildAdd(
                        self.builder,
                        len,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"newlen".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, new_len, len_field);

                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_int_remove" => {
                    // O(n) remove: shift elements left after removing
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;

                    // Get data ptr
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"data".as_ptr());

                    // Get len
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
                    let len = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"len".as_ptr());

                    // Get element at index (to return)
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [index].as_mut_ptr(),
                        1,
                        c"elem".as_ptr(),
                    );
                    let removed_val =
                        LLVMBuildLoad2(self.builder, i64_ty, elem_ptr, c"removed".as_ptr());

                    // Calculate bytes to move: (len - index - 1) * 8
                    let index_plus_one = LLVMBuildAdd(
                        self.builder,
                        index,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"idx1".as_ptr(),
                    );
                    let elements_to_move =
                        LLVMBuildSub(self.builder, len, index_plus_one, c"tomove".as_ptr());
                    let bytes_to_move = LLVMBuildMul(
                        self.builder,
                        elements_to_move,
                        LLVMConstInt(i64_ty, 8, 0),
                        c"bytes".as_ptr(),
                    );

                    // Source: data[index+1], Dest: data[index]
                    let src_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [index_plus_one].as_mut_ptr(),
                        1,
                        c"src".as_ptr(),
                    );

                    // memmove(dst, src, bytes)
                    let memmove_fn = *self
                        .functions
                        .get("memmove")
                        .ok_or_else(|| CompilerError::codegen_error("Missing memmove"))?;
                    let memmove_ty = LLVMGlobalGetValueType(memmove_fn);
                    let src_i8 = LLVMBuildBitCast(self.builder, src_ptr, i8_ptr_ty, c"".as_ptr());
                    let dst_i8 = LLVMBuildBitCast(self.builder, elem_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        memmove_ty,
                        memmove_fn,
                        [dst_i8, src_i8, bytes_to_move].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    // Decrement len
                    let new_len = LLVMBuildSub(
                        self.builder,
                        len,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"newlen".as_ptr(),
                    );
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
            let trap_bb =
                LLVMAppendBasicBlockInContext(self.context, current_fn, c"null.trap".as_ptr());
            let ok_bb =
                LLVMAppendBasicBlockInContext(self.context, current_fn, c"null.ok".as_ptr());

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
            let abort_fn = *self
                .functions
                .get("abort")
                .ok_or_else(|| CompilerError::codegen_error("Missing abort"))?;
            let abort_ty = LLVMGlobalGetValueType(abort_fn);
            LLVMBuildCall2(
                self.builder,
                abort_ty,
                abort_fn,
                std::ptr::null_mut(),
                0,
                c"".as_ptr(),
            );
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

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
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
                    let result =
                        LLVMBuildZExt(self.builder, byte_val, i64_ty, c"char.int".as_ptr());
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

                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let memcpy_fn = *self
                        .functions
                        .get("memcpy")
                        .ok_or_else(|| CompilerError::codegen_error("Missing memcpy"))?;
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

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let strcpy_fn = *self
                        .functions
                        .get("strcpy")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcpy"))?;
                    let strcpy_ty = LLVMGlobalGetValueType(strcpy_fn);
                    let strcat_fn = *self
                        .functions
                        .get("strcat")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcat"))?;
                    let strcat_ty = LLVMGlobalGetValueType(strcat_fn);

                    let len_a = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [a].as_mut_ptr(),
                        1,
                        c"len.a".as_ptr(),
                    );
                    let len_b = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [b].as_mut_ptr(),
                        1,
                        c"len.b".as_ptr(),
                    );

                    let total = LLVMBuildAdd(self.builder, len_a, len_b, c"total.len".as_ptr());
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let alloc_size = LLVMBuildAdd(self.builder, total, one, c"alloc.size".as_ptr());

                    let new_str = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [alloc_size].as_mut_ptr(),
                        1,
                        c"concat.ptr".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        strcpy_ty,
                        strcpy_fn,
                        [new_str, a].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        strcat_ty,
                        strcat_fn,
                        [new_str, b].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    Ok(Some(new_str))
                }

                "str_eq" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;

                    // Trap on null pointers
                    self.emit_null_check(a, "str_eq: null first string")?;
                    self.emit_null_check(b, "str_eq: null second string")?;

                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let i32_ty = LLVMInt32TypeInContext(self.context);

                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [a, b].as_mut_ptr(),
                        2,
                        c"strcmp".as_ptr(),
                    );
                    let zero = LLVMConstInt(i32_ty, 0, 0);
                    let result = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        zero,
                        c"str.eq".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "str_ne" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;

                    // Trap on null pointers
                    self.emit_null_check(a, "str_ne: null first string")?;
                    self.emit_null_check(b, "str_ne: null second string")?;

                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let i32_ty = LLVMInt32TypeInContext(self.context);

                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [a, b].as_mut_ptr(),
                        2,
                        c"strcmp".as_ptr(),
                    );
                    let zero = LLVMConstInt(i32_ty, 0, 0);
                    let result = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntNE,
                        cmp_result,
                        zero,
                        c"str.ne".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "bytes_eq" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;

                    // Trap on null pointers
                    self.emit_null_check(a, "bytes_eq: null first bytes")?;
                    self.emit_null_check(b, "bytes_eq: null second bytes")?;

                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let i32_ty = LLVMInt32TypeInContext(self.context);

                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [a, b].as_mut_ptr(),
                        2,
                        c"strcmp".as_ptr(),
                    );
                    let zero = LLVMConstInt(i32_ty, 0, 0);
                    let result = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        zero,
                        c"bytes.eq".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "str_trim" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    self.emit_null_check(s, "str_trim: null string")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    // Get strlen
                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"len".as_ptr(),
                    );

                    // Find start (skip leading whitespace)
                    let start_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"trim.start".as_ptr(),
                    );
                    let start_loop = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"trim.start.loop".as_ptr(),
                    );
                    let start_done = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"trim.start.done".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, start_bb);
                    LLVMPositionBuilderAtEnd(self.builder, start_bb);
                    LLVMBuildBr(self.builder, start_loop);

                    LLVMPositionBuilderAtEnd(self.builder, start_loop);
                    let start_phi = LLVMBuildPhi(self.builder, i64_ty, c"start.idx".as_ptr());
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    LLVMAddIncoming(start_phi, [zero].as_mut_ptr(), [start_bb].as_mut_ptr(), 1);

                    let char_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [start_phi].as_mut_ptr(),
                        1,
                        c"char.ptr".as_ptr(),
                    );
                    let ch = LLVMBuildLoad2(self.builder, i8_ty, char_ptr, c"ch".as_ptr());
                    let ch_i64 = LLVMBuildZExt(self.builder, ch, i64_ty, c"ch.i64".as_ptr());

                    // Check if whitespace (space=32, tab=9, newline=10, return=13)
                    let is_space = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        ch_i64,
                        LLVMConstInt(i64_ty, 32, 0),
                        c"is.space".as_ptr(),
                    );
                    let is_tab = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        ch_i64,
                        LLVMConstInt(i64_ty, 9, 0),
                        c"is.tab".as_ptr(),
                    );
                    let is_newline = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        ch_i64,
                        LLVMConstInt(i64_ty, 10, 0),
                        c"is.newline".as_ptr(),
                    );
                    let is_return = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        ch_i64,
                        LLVMConstInt(i64_ty, 13, 0),
                        c"is.return".as_ptr(),
                    );

                    let ws1 = LLVMBuildOr(self.builder, is_space, is_tab, c"ws1".as_ptr());
                    let ws2 = LLVMBuildOr(self.builder, is_newline, is_return, c"ws2".as_ptr());
                    let is_ws = LLVMBuildOr(self.builder, ws1, ws2, c"is.ws".as_ptr());

                    let at_end = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                        start_phi,
                        len,
                        c"at.end".as_ptr(),
                    );
                    let should_continue = LLVMBuildAnd(
                        self.builder,
                        is_ws,
                        LLVMBuildNot(self.builder, at_end, c"not.end".as_ptr()),
                        c"continue".as_ptr(),
                    );

                    let next_start = LLVMBuildAdd(
                        self.builder,
                        start_phi,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"next.start".as_ptr(),
                    );
                    LLVMAddIncoming(
                        start_phi,
                        [next_start].as_mut_ptr(),
                        [start_loop].as_mut_ptr(),
                        1,
                    );

                    LLVMBuildCondBr(self.builder, should_continue, start_loop, start_done);
                    LLVMPositionBuilderAtEnd(self.builder, start_done);

                    // Find end (skip trailing whitespace) - work backwards from len
                    let end_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"trim.end".as_ptr(),
                    );
                    let end_loop = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"trim.end.loop".as_ptr(),
                    );
                    let end_done = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"trim.end.done".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, end_bb);
                    LLVMPositionBuilderAtEnd(self.builder, end_bb);
                    LLVMBuildBr(self.builder, end_loop);

                    LLVMPositionBuilderAtEnd(self.builder, end_loop);
                    let end_phi = LLVMBuildPhi(self.builder, i64_ty, c"end.idx".as_ptr());
                    LLVMAddIncoming(end_phi, [len].as_mut_ptr(), [end_bb].as_mut_ptr(), 1);

                    let is_at_start = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntULE,
                        end_phi,
                        start_phi,
                        c"at.start".as_ptr(),
                    );

                    let prev_end = LLVMBuildSub(
                        self.builder,
                        end_phi,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"prev.end".as_ptr(),
                    );
                    let end_char_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [prev_end].as_mut_ptr(),
                        1,
                        c"end.char.ptr".as_ptr(),
                    );
                    let end_ch =
                        LLVMBuildLoad2(self.builder, i8_ty, end_char_ptr, c"end.ch".as_ptr());
                    let end_ch_i64 =
                        LLVMBuildZExt(self.builder, end_ch, i64_ty, c"end.ch.i64".as_ptr());

                    let end_is_space = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        end_ch_i64,
                        LLVMConstInt(i64_ty, 32, 0),
                        c"end.is.space".as_ptr(),
                    );
                    let end_is_tab = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        end_ch_i64,
                        LLVMConstInt(i64_ty, 9, 0),
                        c"end.is.tab".as_ptr(),
                    );
                    let end_is_newline = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        end_ch_i64,
                        LLVMConstInt(i64_ty, 10, 0),
                        c"end.is.newline".as_ptr(),
                    );
                    let end_is_return = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        end_ch_i64,
                        LLVMConstInt(i64_ty, 13, 0),
                        c"end.is.return".as_ptr(),
                    );

                    let end_ws1 =
                        LLVMBuildOr(self.builder, end_is_space, end_is_tab, c"end.ws1".as_ptr());
                    let end_ws2 = LLVMBuildOr(
                        self.builder,
                        end_is_newline,
                        end_is_return,
                        c"end.ws2".as_ptr(),
                    );
                    let end_is_ws =
                        LLVMBuildOr(self.builder, end_ws1, end_ws2, c"end.is.ws".as_ptr());

                    let end_should_continue = LLVMBuildAnd(
                        self.builder,
                        end_is_ws,
                        LLVMBuildNot(self.builder, is_at_start, c"not.at.start".as_ptr()),
                        c"end.continue".as_ptr(),
                    );

                    LLVMAddIncoming(end_phi, [prev_end].as_mut_ptr(), [end_loop].as_mut_ptr(), 1);
                    LLVMBuildCondBr(self.builder, end_should_continue, end_loop, end_done);
                    LLVMPositionBuilderAtEnd(self.builder, end_done);

                    // Allocate new string and copy trimmed portion
                    let trimmed_len =
                        LLVMBuildSub(self.builder, end_phi, start_phi, c"trimmed.len".as_ptr());
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let alloc_size =
                        LLVMBuildAdd(self.builder, trimmed_len, one, c"alloc.size".as_ptr());

                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let new_str = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [alloc_size].as_mut_ptr(),
                        1,
                        c"trimmed.ptr".as_ptr(),
                    );

                    let memcpy_fn = *self
                        .functions
                        .get("memcpy")
                        .ok_or_else(|| CompilerError::codegen_error("Missing memcpy"))?;
                    let memcpy_ty = LLVMGlobalGetValueType(memcpy_fn);
                    let src_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [start_phi].as_mut_ptr(),
                        1,
                        c"src.ptr".as_ptr(),
                    );

                    LLVMBuildCall2(
                        self.builder,
                        memcpy_ty,
                        memcpy_fn,
                        [new_str, src_ptr, trimmed_len].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    let null_pos = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        new_str,
                        [trimmed_len].as_mut_ptr(),
                        1,
                        c"null.pos".as_ptr(),
                    );
                    let zero_byte = LLVMConstInt(i8_ty, 0, 0);
                    LLVMBuildStore(self.builder, zero_byte, null_pos);

                    Ok(Some(new_str))
                }

                "str_contains" => {
                    let haystack = self.codegen_expression(&arguments[0])?;
                    let needle = self.codegen_expression(&arguments[1])?;

                    self.emit_null_check(haystack, "str_contains: null haystack")?;
                    self.emit_null_check(needle, "str_contains: null needle")?;

                    let strstr_fn = *self
                        .functions
                        .get("strstr")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strstr"))?;
                    let strstr_ty = LLVMGlobalGetValueType(strstr_fn);

                    let result_ptr = LLVMBuildCall2(
                        self.builder,
                        strstr_ty,
                        strstr_fn,
                        [haystack, needle].as_mut_ptr(),
                        2,
                        c"strstr.result".as_ptr(),
                    );

                    let null_ptr =
                        LLVMConstNull(LLVMPointerType(LLVMInt8TypeInContext(self.context), 0));
                    let is_not_null = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntNE,
                        result_ptr,
                        null_ptr,
                        c"contains".as_ptr(),
                    );

                    Ok(Some(is_not_null))
                }

                "str_starts_with" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    let prefix = self.codegen_expression(&arguments[1])?;

                    self.emit_null_check(s, "str_starts_with: null string")?;
                    self.emit_null_check(prefix, "str_starts_with: null prefix")?;

                    let _i64_ty = LLVMInt64TypeInContext(self.context);

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let prefix_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [prefix].as_mut_ptr(),
                        1,
                        c"prefix.len".as_ptr(),
                    );

                    let strncmp_fn = *self
                        .functions
                        .get("strncmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strncmp"))?;
                    let strncmp_ty = LLVMGlobalGetValueType(strncmp_fn);

                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strncmp_ty,
                        strncmp_fn,
                        [s, prefix, prefix_len].as_mut_ptr(),
                        3,
                        c"strncmp".as_ptr(),
                    );

                    // strncmp is declared to return i64 in our LLVM module
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    let starts_with = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        zero,
                        c"starts.with".as_ptr(),
                    );

                    Ok(Some(starts_with))
                }

                "str_ends_with" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    let suffix = self.codegen_expression(&arguments[1])?;

                    self.emit_null_check(s, "str_ends_with: null string")?;
                    self.emit_null_check(suffix, "str_ends_with: null suffix")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let _i64_ty = LLVMInt64TypeInContext(self.context);

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);

                    let s_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"s.len".as_ptr(),
                    );
                    let suffix_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [suffix].as_mut_ptr(),
                        1,
                        c"suffix.len".as_ptr(),
                    );

                    // Check if suffix is longer than string
                    let suffix_too_long = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntUGT,
                        suffix_len,
                        s_len,
                        c"suffix.too.long".as_ptr(),
                    );

                    let check_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"check.suffix".as_ptr(),
                    );
                    let false_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"false.result".as_ptr(),
                    );
                    let merge_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"merge".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, suffix_too_long, false_bb, check_bb);

                    // Check suffix
                    LLVMPositionBuilderAtEnd(self.builder, check_bb);
                    let offset = LLVMBuildSub(self.builder, s_len, suffix_len, c"offset".as_ptr());
                    let suffix_start = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [offset].as_mut_ptr(),
                        1,
                        c"suffix.start".as_ptr(),
                    );

                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);

                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [suffix_start, suffix].as_mut_ptr(),
                        2,
                        c"strcmp".as_ptr(),
                    );

                    let zero = LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0);
                    let ends_with = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        zero,
                        c"ends.with".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, merge_bb);

                    // False result
                    LLVMPositionBuilderAtEnd(self.builder, false_bb);
                    let false_val = LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0);
                    LLVMBuildBr(self.builder, merge_bb);

                    // Merge
                    LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                    let result_phi = LLVMBuildPhi(
                        self.builder,
                        LLVMInt1TypeInContext(self.context),
                        c"result".as_ptr(),
                    );
                    LLVMAddIncoming(
                        result_phi,
                        [ends_with].as_mut_ptr(),
                        [check_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        result_phi,
                        [false_val].as_mut_ptr(),
                        [false_bb].as_mut_ptr(),
                        1,
                    );

                    Ok(Some(result_phi))
                }

                "str_is_valid_utf8" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    self.emit_null_check(s, "str_is_valid_utf8: null string")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"len".as_ptr(),
                    );

                    // Simple UTF-8 validation loop
                    let entry_bb = LLVMGetInsertBlock(self.builder);
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"utf8.loop".as_ptr(),
                    );
                    let valid_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"utf8.valid".as_ptr(),
                    );
                    let invalid_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"utf8.invalid".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);

                    let idx_phi = LLVMBuildPhi(self.builder, i64_ty, c"idx".as_ptr());
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    LLVMAddIncoming(idx_phi, [zero].as_mut_ptr(), [entry_bb].as_mut_ptr(), 1);

                    // Check if done
                    let done = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                        idx_phi,
                        len,
                        c"done".as_ptr(),
                    );

                    let check_byte_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"check.byte".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, done, valid_bb, check_byte_bb);
                    LLVMPositionBuilderAtEnd(self.builder, check_byte_bb);

                    // Load byte
                    let byte_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [idx_phi].as_mut_ptr(),
                        1,
                        c"byte.ptr".as_ptr(),
                    );
                    let byte = LLVMBuildLoad2(self.builder, i8_ty, byte_ptr, c"byte".as_ptr());
                    let byte_i64 = LLVMBuildZExt(self.builder, byte, i64_ty, c"byte.i64".as_ptr());

                    // Simple check: bytes must be < 128 (ASCII) or valid UTF-8 start bytes
                    // For simplicity, we'll just check that high bit patterns are valid
                    // This is a simplified check - full UTF-8 validation is more complex
                    let is_ascii = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntULT,
                        byte_i64,
                        LLVMConstInt(i64_ty, 128, 0),
                        c"is.ascii".as_ptr(),
                    );

                    // Check for invalid byte (0xC0, 0xC1, 0xF5-0xFF are invalid in UTF-8)
                    let is_c0 = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        byte_i64,
                        LLVMConstInt(i64_ty, 0xC0, 0),
                        c"is.c0".as_ptr(),
                    );
                    let is_c1 = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        byte_i64,
                        LLVMConstInt(i64_ty, 0xC1, 0),
                        c"is.c1".as_ptr(),
                    );
                    let is_invalid =
                        LLVMBuildOr(self.builder, is_c0, is_c1, c"is.invalid".as_ptr());

                    let byte_valid = LLVMBuildOr(
                        self.builder,
                        is_ascii,
                        LLVMBuildNot(self.builder, is_invalid, c"not.invalid".as_ptr()),
                        c"byte.valid".as_ptr(),
                    );

                    let next_idx = LLVMBuildAdd(
                        self.builder,
                        idx_phi,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"next.idx".as_ptr(),
                    );
                    LLVMAddIncoming(
                        idx_phi,
                        [next_idx].as_mut_ptr(),
                        [check_byte_bb].as_mut_ptr(),
                        1,
                    );

                    LLVMBuildCondBr(self.builder, byte_valid, loop_bb, invalid_bb);

                    // Valid path
                    LLVMPositionBuilderAtEnd(self.builder, valid_bb);
                    let true_val = LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0);
                    let merge_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"merge".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, merge_bb);

                    // Invalid path
                    LLVMPositionBuilderAtEnd(self.builder, invalid_bb);
                    let false_val = LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0);
                    LLVMBuildBr(self.builder, merge_bb);

                    // Merge
                    LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                    let result_phi = LLVMBuildPhi(
                        self.builder,
                        LLVMInt1TypeInContext(self.context),
                        c"result".as_ptr(),
                    );
                    LLVMAddIncoming(
                        result_phi,
                        [true_val].as_mut_ptr(),
                        [valid_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        result_phi,
                        [false_val].as_mut_ptr(),
                        [invalid_bb].as_mut_ptr(),
                        1,
                    );

                    Ok(Some(result_phi))
                }

                "str_to_upper" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    self.emit_null_check(s, "str_to_upper: null string")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"len".as_ptr(),
                    );

                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let alloc_size = LLVMBuildAdd(self.builder, len, one, c"alloc.size".as_ptr());

                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let new_str = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [alloc_size].as_mut_ptr(),
                        1,
                        c"upper.ptr".as_ptr(),
                    );

                    // Loop through string converting to uppercase
                    let entry_bb = LLVMGetInsertBlock(self.builder);
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"upper.loop".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"upper.done".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);

                    let idx_phi = LLVMBuildPhi(self.builder, i64_ty, c"idx".as_ptr());
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    LLVMAddIncoming(idx_phi, [zero].as_mut_ptr(), [entry_bb].as_mut_ptr(), 1);

                    let done = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                        idx_phi,
                        len,
                        c"done".as_ptr(),
                    );

                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"upper.body".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, done, done_bb, body_bb);
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);

                    // Load character
                    let src_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [idx_phi].as_mut_ptr(),
                        1,
                        c"src.ptr".as_ptr(),
                    );
                    let ch = LLVMBuildLoad2(self.builder, i8_ty, src_ptr, c"ch".as_ptr());
                    let ch_i64 = LLVMBuildZExt(self.builder, ch, i64_ty, c"ch.i64".as_ptr());

                    // Check if lowercase (a-z: 97-122)
                    let is_lower = LLVMBuildAnd(
                        self.builder,
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                            ch_i64,
                            LLVMConstInt(i64_ty, 97, 0),
                            c"ge.a".as_ptr(),
                        ),
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntULE,
                            ch_i64,
                            LLVMConstInt(i64_ty, 122, 0),
                            c"le.z".as_ptr(),
                        ),
                        c"is.lower".as_ptr(),
                    );

                    // Convert to uppercase by subtracting 32
                    let upper_ch_i64 = LLVMBuildSub(
                        self.builder,
                        ch_i64,
                        LLVMConstInt(i64_ty, 32, 0),
                        c"upper.ch.i64".as_ptr(),
                    );
                    let result_ch_i64 = LLVMBuildSelect(
                        self.builder,
                        is_lower,
                        upper_ch_i64,
                        ch_i64,
                        c"result.ch.i64".as_ptr(),
                    );
                    let result_ch =
                        LLVMBuildTrunc(self.builder, result_ch_i64, i8_ty, c"result.ch".as_ptr());

                    // Store character
                    let dst_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        new_str,
                        [idx_phi].as_mut_ptr(),
                        1,
                        c"dst.ptr".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, result_ch, dst_ptr);

                    let next_idx = LLVMBuildAdd(self.builder, idx_phi, one, c"next.idx".as_ptr());
                    LLVMAddIncoming(idx_phi, [next_idx].as_mut_ptr(), [body_bb].as_mut_ptr(), 1);
                    LLVMBuildBr(self.builder, loop_bb);

                    // Done - null terminate
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
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

                "str_to_lower" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    self.emit_null_check(s, "str_to_lower: null string")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"len".as_ptr(),
                    );

                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let alloc_size = LLVMBuildAdd(self.builder, len, one, c"alloc.size".as_ptr());

                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let new_str = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [alloc_size].as_mut_ptr(),
                        1,
                        c"lower.ptr".as_ptr(),
                    );

                    // Loop through string converting to lowercase
                    let entry_bb = LLVMGetInsertBlock(self.builder);
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"lower.loop".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"lower.done".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);

                    let idx_phi = LLVMBuildPhi(self.builder, i64_ty, c"idx".as_ptr());
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    LLVMAddIncoming(idx_phi, [zero].as_mut_ptr(), [entry_bb].as_mut_ptr(), 1);

                    let done = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                        idx_phi,
                        len,
                        c"done".as_ptr(),
                    );

                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"lower.body".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, done, done_bb, body_bb);
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);

                    // Load character
                    let src_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [idx_phi].as_mut_ptr(),
                        1,
                        c"src.ptr".as_ptr(),
                    );
                    let ch = LLVMBuildLoad2(self.builder, i8_ty, src_ptr, c"ch".as_ptr());
                    let ch_i64 = LLVMBuildZExt(self.builder, ch, i64_ty, c"ch.i64".as_ptr());

                    // Check if uppercase (A-Z: 65-90)
                    let is_upper = LLVMBuildAnd(
                        self.builder,
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                            ch_i64,
                            LLVMConstInt(i64_ty, 65, 0),
                            c"ge.A".as_ptr(),
                        ),
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntULE,
                            ch_i64,
                            LLVMConstInt(i64_ty, 90, 0),
                            c"le.Z".as_ptr(),
                        ),
                        c"is.upper".as_ptr(),
                    );

                    // Convert to lowercase by adding 32
                    let lower_ch_i64 = LLVMBuildAdd(
                        self.builder,
                        ch_i64,
                        LLVMConstInt(i64_ty, 32, 0),
                        c"lower.ch.i64".as_ptr(),
                    );
                    let result_ch_i64 = LLVMBuildSelect(
                        self.builder,
                        is_upper,
                        lower_ch_i64,
                        ch_i64,
                        c"result.ch.i64".as_ptr(),
                    );
                    let result_ch =
                        LLVMBuildTrunc(self.builder, result_ch_i64, i8_ty, c"result.ch".as_ptr());

                    // Store character
                    let dst_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        new_str,
                        [idx_phi].as_mut_ptr(),
                        1,
                        c"dst.ptr".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, result_ch, dst_ptr);

                    let next_idx = LLVMBuildAdd(self.builder, idx_phi, one, c"next.idx".as_ptr());
                    LLVMAddIncoming(idx_phi, [next_idx].as_mut_ptr(), [body_bb].as_mut_ptr(), 1);
                    LLVMBuildBr(self.builder, loop_bb);

                    // Done - null terminate
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
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

                "str_index_of" => {
                    let haystack = self.codegen_expression(&arguments[0])?;
                    let needle = self.codegen_expression(&arguments[1])?;

                    self.emit_null_check(haystack, "str_index_of: null haystack")?;
                    self.emit_null_check(needle, "str_index_of: null needle")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    let strstr_fn = *self
                        .functions
                        .get("strstr")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strstr"))?;
                    let strstr_ty = LLVMGlobalGetValueType(strstr_fn);

                    let result_ptr = LLVMBuildCall2(
                        self.builder,
                        strstr_ty,
                        strstr_fn,
                        [haystack, needle].as_mut_ptr(),
                        2,
                        c"strstr.result".as_ptr(),
                    );

                    // Check if found
                    let null_ptr = LLVMConstNull(LLVMPointerType(i8_ty, 0));
                    let found = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntNE,
                        result_ptr,
                        null_ptr,
                        c"found".as_ptr(),
                    );

                    let calc_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"calc.index".as_ptr(),
                    );
                    let not_found_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"not.found".as_ptr(),
                    );
                    let merge_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"merge".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, found, calc_bb, not_found_bb);

                    // Calculate index: result_ptr - haystack
                    LLVMPositionBuilderAtEnd(self.builder, calc_bb);
                    let haystack_int =
                        LLVMBuildPtrToInt(self.builder, haystack, i64_ty, c"haystack.int".as_ptr());
                    let result_int =
                        LLVMBuildPtrToInt(self.builder, result_ptr, i64_ty, c"result.int".as_ptr());
                    let index =
                        LLVMBuildSub(self.builder, result_int, haystack_int, c"index".as_ptr());
                    LLVMBuildBr(self.builder, merge_bb);

                    // Not found: return -1
                    LLVMPositionBuilderAtEnd(self.builder, not_found_bb);
                    let neg_one = LLVMConstInt(i64_ty, u64::MAX, 1);
                    LLVMBuildBr(self.builder, merge_bb);

                    // Merge
                    LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                    let result_phi = LLVMBuildPhi(self.builder, i64_ty, c"result".as_ptr());
                    LLVMAddIncoming(result_phi, [index].as_mut_ptr(), [calc_bb].as_mut_ptr(), 1);
                    LLVMAddIncoming(
                        result_phi,
                        [neg_one].as_mut_ptr(),
                        [not_found_bb].as_mut_ptr(),
                        1,
                    );

                    Ok(Some(result_phi))
                }

                "str_char_count" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    self.emit_null_check(s, "str_char_count: null string")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let byte_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"byte.len".as_ptr(),
                    );

                    // Count UTF-8 characters by counting non-continuation bytes
                    let entry_bb = LLVMGetInsertBlock(self.builder);
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"count.loop".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"count.done".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);

                    let idx_phi = LLVMBuildPhi(self.builder, i64_ty, c"idx".as_ptr());
                    let count_phi = LLVMBuildPhi(self.builder, i64_ty, c"count".as_ptr());
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    LLVMAddIncoming(idx_phi, [zero].as_mut_ptr(), [entry_bb].as_mut_ptr(), 1);
                    LLVMAddIncoming(count_phi, [zero].as_mut_ptr(), [entry_bb].as_mut_ptr(), 1);

                    let done = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                        idx_phi,
                        byte_len,
                        c"done".as_ptr(),
                    );

                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"count.body".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, done, done_bb, body_bb);
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);

                    // Load byte
                    let byte_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [idx_phi].as_mut_ptr(),
                        1,
                        c"byte.ptr".as_ptr(),
                    );
                    let byte = LLVMBuildLoad2(self.builder, i8_ty, byte_ptr, c"byte".as_ptr());
                    let byte_i64 = LLVMBuildZExt(self.builder, byte, i64_ty, c"byte.i64".as_ptr());

                    // Check if NOT a continuation byte (continuation bytes start with 10xxxxxx = 0x80-0xBF)
                    let is_continuation = LLVMBuildAnd(
                        self.builder,
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                            byte_i64,
                            LLVMConstInt(i64_ty, 0x80, 0),
                            c"ge.80".as_ptr(),
                        ),
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntULE,
                            byte_i64,
                            LLVMConstInt(i64_ty, 0xBF, 0),
                            c"le.BF".as_ptr(),
                        ),
                        c"is.cont".as_ptr(),
                    );

                    let is_char_start =
                        LLVMBuildNot(self.builder, is_continuation, c"is.char.start".as_ptr());
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let count_inc = LLVMBuildSelect(
                        self.builder,
                        is_char_start,
                        one,
                        zero,
                        c"count.inc".as_ptr(),
                    );
                    let new_count =
                        LLVMBuildAdd(self.builder, count_phi, count_inc, c"new.count".as_ptr());

                    let next_idx = LLVMBuildAdd(self.builder, idx_phi, one, c"next.idx".as_ptr());
                    LLVMAddIncoming(idx_phi, [next_idx].as_mut_ptr(), [body_bb].as_mut_ptr(), 1);
                    LLVMAddIncoming(
                        count_phi,
                        [new_count].as_mut_ptr(),
                        [body_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMBuildBr(self.builder, loop_bb);

                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    Ok(Some(count_phi))
                }

                "str_char_at_utf8" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    let char_idx = self.codegen_expression(&arguments[1])?;

                    self.emit_null_check(s, "str_char_at_utf8: null string")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let byte_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"byte.len".as_ptr(),
                    );

                    // Find the byte offset of the nth UTF-8 character
                    let entry_bb = LLVMGetInsertBlock(self.builder);
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"find.loop".as_ptr(),
                    );
                    let found_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"found".as_ptr(),
                    );
                    let not_found_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"not.found".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);

                    let byte_idx_phi = LLVMBuildPhi(self.builder, i64_ty, c"byte.idx".as_ptr());
                    let char_count_phi = LLVMBuildPhi(self.builder, i64_ty, c"char.count".as_ptr());
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    LLVMAddIncoming(
                        byte_idx_phi,
                        [zero].as_mut_ptr(),
                        [entry_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        char_count_phi,
                        [zero].as_mut_ptr(),
                        [entry_bb].as_mut_ptr(),
                        1,
                    );

                    // Check if we've found the character
                    let found_char = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        char_count_phi,
                        char_idx,
                        c"found.char".as_ptr(),
                    );

                    let check_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"check".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, found_char, found_bb, check_bb);
                    LLVMPositionBuilderAtEnd(self.builder, check_bb);

                    // Check if we've reached end of string
                    let at_end = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                        byte_idx_phi,
                        byte_len,
                        c"at.end".as_ptr(),
                    );

                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"body".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, at_end, not_found_bb, body_bb);
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);

                    // Load byte and check if it's a character start
                    let byte_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [byte_idx_phi].as_mut_ptr(),
                        1,
                        c"byte.ptr".as_ptr(),
                    );
                    let byte = LLVMBuildLoad2(self.builder, i8_ty, byte_ptr, c"byte".as_ptr());
                    let byte_i64 = LLVMBuildZExt(self.builder, byte, i64_ty, c"byte.i64".as_ptr());

                    let is_continuation = LLVMBuildAnd(
                        self.builder,
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                            byte_i64,
                            LLVMConstInt(i64_ty, 0x80, 0),
                            c"ge.80".as_ptr(),
                        ),
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntULE,
                            byte_i64,
                            LLVMConstInt(i64_ty, 0xBF, 0),
                            c"le.BF".as_ptr(),
                        ),
                        c"is.cont".as_ptr(),
                    );

                    let is_char_start =
                        LLVMBuildNot(self.builder, is_continuation, c"is.char.start".as_ptr());
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let count_inc = LLVMBuildSelect(
                        self.builder,
                        is_char_start,
                        one,
                        zero,
                        c"count.inc".as_ptr(),
                    );
                    let new_count = LLVMBuildAdd(
                        self.builder,
                        char_count_phi,
                        count_inc,
                        c"new.count".as_ptr(),
                    );

                    let next_byte_idx =
                        LLVMBuildAdd(self.builder, byte_idx_phi, one, c"next.byte.idx".as_ptr());
                    LLVMAddIncoming(
                        byte_idx_phi,
                        [next_byte_idx].as_mut_ptr(),
                        [body_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        char_count_phi,
                        [new_count].as_mut_ptr(),
                        [body_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMBuildBr(self.builder, loop_bb);

                    // Found: return the byte at byte_idx_phi
                    LLVMPositionBuilderAtEnd(self.builder, found_bb);
                    let found_byte_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [byte_idx_phi].as_mut_ptr(),
                        1,
                        c"found.byte.ptr".as_ptr(),
                    );
                    let found_byte =
                        LLVMBuildLoad2(self.builder, i8_ty, found_byte_ptr, c"found.byte".as_ptr());
                    let found_result =
                        LLVMBuildZExt(self.builder, found_byte, i64_ty, c"found.result".as_ptr());

                    let merge_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"merge".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, merge_bb);

                    // Not found: return 0
                    LLVMPositionBuilderAtEnd(self.builder, not_found_bb);
                    LLVMBuildBr(self.builder, merge_bb);

                    // Merge
                    LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                    let result_phi = LLVMBuildPhi(self.builder, i64_ty, c"result".as_ptr());
                    LLVMAddIncoming(
                        result_phi,
                        [found_result].as_mut_ptr(),
                        [found_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        result_phi,
                        [zero].as_mut_ptr(),
                        [not_found_bb].as_mut_ptr(),
                        1,
                    );

                    Ok(Some(result_phi))
                }

                "str_split" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    let delim = self.codegen_expression(&arguments[1])?;

                    self.emit_null_check(s, "str_split: null string")?;
                    self.emit_null_check(delim, "str_split: null delimiter")?;

                    // Call C runtime function: VecString* kraken_str_split(const char* s, const char* delim)
                    let kraken_str_split_fn = *self
                        .functions
                        .get("kraken_str_split")
                        .ok_or_else(|| CompilerError::codegen_error("Missing kraken_str_split"))?;
                    let kraken_str_split_ty = LLVMGlobalGetValueType(kraken_str_split_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        kraken_str_split_ty,
                        kraken_str_split_fn,
                        [s, delim].as_mut_ptr(),
                        2,
                        c"result".as_ptr(),
                    );

                    Ok(Some(result))
                }

                "str_split_OLD_BUGGY" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    let delim = self.codegen_expression(&arguments[1])?;

                    self.emit_null_check(s, "str_split: null string")?;
                    self.emit_null_check(delim, "str_split: null delimiter")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    // Get delimiter length
                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let delim_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [delim].as_mut_ptr(),
                        1,
                        c"delim.len".as_ptr(),
                    );
                    let s_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"s.len".as_ptr(),
                    );

                    // Create new VecString using vec_string_new
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);

                    let vec_struct = LLVMBuildCall2(
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

                    let str_ptr_ty = LLVMPointerType(i8_ptr_ty, 0);
                    let array_typed =
                        LLVMBuildBitCast(self.builder, array_ptr, str_ptr_ty, c"".as_ptr());
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_struct,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, array_typed, ptr_field);

                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_struct,
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
                        vec_struct,
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

                    // Check if delimiter is empty
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    let delim_empty = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        delim_len,
                        zero,
                        c"delim.empty".as_ptr(),
                    );

                    let empty_delim_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"empty.delim".as_ptr(),
                    );
                    let normal_split_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"normal.split".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"split.done".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, delim_empty, empty_delim_bb, normal_split_bb);

                    // Empty delimiter: add whole string to vec
                    LLVMPositionBuilderAtEnd(self.builder, empty_delim_bb);
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let copy_size = LLVMBuildAdd(self.builder, s_len, one, c"copy.size".as_ptr());
                    let str_copy = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [copy_size].as_mut_ptr(),
                        1,
                        c"str.copy".as_ptr(),
                    );
                    let strcpy_fn = *self
                        .functions
                        .get("strcpy")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcpy"))?;
                    let strcpy_ty = LLVMGlobalGetValueType(strcpy_fn);
                    LLVMBuildCall2(
                        self.builder,
                        strcpy_ty,
                        strcpy_fn,
                        [str_copy, s].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    let data_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"data".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        data_ptr,
                        [zero].as_mut_ptr(),
                        1,
                        c"elem".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, str_copy, elem_ptr);
                    LLVMBuildStore(self.builder, one, len_field);
                    LLVMBuildBr(self.builder, done_bb);

                    // Normal split: use strstr to find occurrences
                    LLVMPositionBuilderAtEnd(self.builder, normal_split_bb);
                    let strstr_fn = *self
                        .functions
                        .get("strstr")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strstr"))?;
                    let strstr_ty = LLVMGlobalGetValueType(strstr_fn);

                    // Split loop - simplified implementation
                    let entry_bb = LLVMGetInsertBlock(self.builder);
                    let split_loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(entry_bb),
                        c"split.loop".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, split_loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, split_loop_bb);

                    let current_ptr_phi =
                        LLVMBuildPhi(self.builder, i8_ptr_ty, c"current.ptr".as_ptr());
                    LLVMAddIncoming(
                        current_ptr_phi,
                        [s].as_mut_ptr(),
                        [entry_bb].as_mut_ptr(),
                        1,
                    );

                    // Find next occurrence
                    let found_ptr = LLVMBuildCall2(
                        self.builder,
                        strstr_ty,
                        strstr_fn,
                        [current_ptr_phi, delim].as_mut_ptr(),
                        2,
                        c"found".as_ptr(),
                    );

                    let null_ptr = LLVMConstNull(i8_ptr_ty);
                    let found = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntNE,
                        found_ptr,
                        null_ptr,
                        c"found".as_ptr(),
                    );

                    let add_part_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"add.part".as_ptr(),
                    );
                    let add_final_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"add.final".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, found, add_part_bb, add_final_bb);

                    // Add part before delimiter
                    LLVMPositionBuilderAtEnd(self.builder, add_part_bb);
                    let current_int = LLVMBuildPtrToInt(
                        self.builder,
                        current_ptr_phi,
                        i64_ty,
                        c"current.int".as_ptr(),
                    );
                    let found_int =
                        LLVMBuildPtrToInt(self.builder, found_ptr, i64_ty, c"found.int".as_ptr());
                    let part_len =
                        LLVMBuildSub(self.builder, found_int, current_int, c"part.len".as_ptr());
                    let part_size =
                        LLVMBuildAdd(self.builder, part_len, one, c"part.size".as_ptr());
                    let part_str = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [part_size].as_mut_ptr(),
                        1,
                        c"part".as_ptr(),
                    );

                    let memcpy_fn = *self
                        .functions
                        .get("memcpy")
                        .ok_or_else(|| CompilerError::codegen_error("Missing memcpy"))?;
                    let memcpy_ty = LLVMGlobalGetValueType(memcpy_fn);
                    LLVMBuildCall2(
                        self.builder,
                        memcpy_ty,
                        memcpy_fn,
                        [part_str, current_ptr_phi, part_len].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );
                    let null_term_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ty,
                        part_str,
                        [part_len].as_mut_ptr(),
                        1,
                        c"null.term".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i8_ty, 0, 0), null_term_ptr);

                    // Add to vector (simplified - assumes capacity)
                    let vec_len =
                        LLVMBuildLoad2(self.builder, i64_ty, len_field, c"vec.len".as_ptr());
                    let data_ptr2 =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"data".as_ptr());
                    let elem_ptr2 = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        data_ptr2,
                        [vec_len].as_mut_ptr(),
                        1,
                        c"elem".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, part_str, elem_ptr2);
                    let new_len = LLVMBuildAdd(self.builder, vec_len, one, c"new.len".as_ptr());
                    LLVMBuildStore(self.builder, new_len, len_field);

                    // Move past delimiter
                    let next_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ty,
                        found_ptr,
                        [delim_len].as_mut_ptr(),
                        1,
                        c"next.ptr".as_ptr(),
                    );
                    LLVMAddIncoming(
                        current_ptr_phi,
                        [next_ptr].as_mut_ptr(),
                        [add_part_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMBuildBr(self.builder, split_loop_bb);

                    // Add final part
                    LLVMPositionBuilderAtEnd(self.builder, add_final_bb);
                    let final_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [current_ptr_phi].as_mut_ptr(),
                        1,
                        c"final.len".as_ptr(),
                    );
                    let final_size =
                        LLVMBuildAdd(self.builder, final_len, one, c"final.size".as_ptr());
                    let final_str = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [final_size].as_mut_ptr(),
                        1,
                        c"final".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        strcpy_ty,
                        strcpy_fn,
                        [final_str, current_ptr_phi].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    let vec_len_final =
                        LLVMBuildLoad2(self.builder, i64_ty, len_field, c"vec.len".as_ptr());
                    let data_ptr_final =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"data".as_ptr());
                    let elem_ptr_final = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        data_ptr_final,
                        [vec_len_final].as_mut_ptr(),
                        1,
                        c"elem".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, final_str, elem_ptr_final);
                    let new_len_final =
                        LLVMBuildAdd(self.builder, vec_len_final, one, c"new.len".as_ptr());
                    LLVMBuildStore(self.builder, new_len_final, len_field);
                    LLVMBuildBr(self.builder, done_bb);

                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    Ok(Some(vec_struct))
                }

                "str_join" => {
                    let vec = self.codegen_expression(&arguments[0])?;
                    let sep = self.codegen_expression(&arguments[1])?;

                    self.emit_null_check(vec, "str_join: null vector")?;
                    self.emit_null_check(sep, "str_join: null separator")?;

                    // Call C runtime function: char* kraken_str_join(VecString* vec, const char* sep)
                    let kraken_str_join_fn = *self
                        .functions
                        .get("kraken_str_join")
                        .ok_or_else(|| CompilerError::codegen_error("Missing kraken_str_join"))?;
                    let kraken_str_join_ty = LLVMGlobalGetValueType(kraken_str_join_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        kraken_str_join_ty,
                        kraken_str_join_fn,
                        [vec, sep].as_mut_ptr(),
                        2,
                        c"result".as_ptr(),
                    );

                    Ok(Some(result))
                }

                "str_join_OLD_BUGGY" => {
                    let vec = self.codegen_expression(&arguments[0])?;
                    let sep = self.codegen_expression(&arguments[1])?;

                    self.emit_null_check(vec, "str_join: null vector")?;
                    self.emit_null_check(sep, "str_join: null separator")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);
                    let str_ptr_ty = LLVMPointerType(i8_ptr_ty, 0);

                    // Get vector length
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec,
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
                    let vec_len =
                        LLVMBuildLoad2(self.builder, i64_ty, len_field, c"vec.len".as_ptr());

                    // Get separator length
                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let sep_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [sep].as_mut_ptr(),
                        1,
                        c"sep.len".as_ptr(),
                    );

                    // Check if empty vector
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    let vec_empty = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        vec_len,
                        zero,
                        c"vec.empty".as_ptr(),
                    );

                    let empty_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"empty.vec".as_ptr(),
                    );
                    let join_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"join".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"done".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, vec_empty, empty_bb, join_bb);

                    // Empty vector: return empty string
                    LLVMPositionBuilderAtEnd(self.builder, empty_bb);
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let empty_str = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 1, 0)].as_mut_ptr(),
                        1,
                        c"empty".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i8_ty, 0, 0), empty_str);
                    LLVMBuildBr(self.builder, done_bb);

                    // Join: calculate total size and build result
                    LLVMPositionBuilderAtEnd(self.builder, join_bb);

                    // Get data pointer
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"data".as_ptr());

                    // Calculate total size (sum of all string lengths + separators)
                    let calc_entry = LLVMGetInsertBlock(self.builder);
                    let calc_loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(calc_entry),
                        c"calc.loop".as_ptr(),
                    );
                    let calc_done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(calc_entry),
                        c"calc.done".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, calc_loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, calc_loop_bb);

                    let idx_phi = LLVMBuildPhi(self.builder, i64_ty, c"idx".as_ptr());
                    let size_phi = LLVMBuildPhi(self.builder, i64_ty, c"size".as_ptr());
                    LLVMAddIncoming(idx_phi, [zero].as_mut_ptr(), [calc_entry].as_mut_ptr(), 1);
                    LLVMAddIncoming(size_phi, [zero].as_mut_ptr(), [calc_entry].as_mut_ptr(), 1);

                    let done_calc = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                        idx_phi,
                        vec_len,
                        c"done.calc".as_ptr(),
                    );

                    let calc_body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"calc.body".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, done_calc, calc_done_bb, calc_body_bb);
                    LLVMPositionBuilderAtEnd(self.builder, calc_body_bb);

                    // Get string at index
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        data_ptr,
                        [idx_phi].as_mut_ptr(),
                        1,
                        c"elem".as_ptr(),
                    );
                    let str_at_idx =
                        LLVMBuildLoad2(self.builder, i8_ptr_ty, elem_ptr, c"str".as_ptr());
                    let str_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [str_at_idx].as_mut_ptr(),
                        1,
                        c"str.len".as_ptr(),
                    );

                    let new_size =
                        LLVMBuildAdd(self.builder, size_phi, str_len, c"new.size".as_ptr());

                    // Add separator length if not last element
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let next_idx = LLVMBuildAdd(self.builder, idx_phi, one, c"next.idx".as_ptr());
                    let is_last = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        next_idx,
                        vec_len,
                        c"is.last".as_ptr(),
                    );
                    let sep_add =
                        LLVMBuildSelect(self.builder, is_last, zero, sep_len, c"sep.add".as_ptr());
                    let final_size =
                        LLVMBuildAdd(self.builder, new_size, sep_add, c"final.size".as_ptr());

                    LLVMAddIncoming(
                        idx_phi,
                        [next_idx].as_mut_ptr(),
                        [calc_body_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        size_phi,
                        [final_size].as_mut_ptr(),
                        [calc_body_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMBuildBr(self.builder, calc_loop_bb);

                    // Allocate result buffer
                    LLVMPositionBuilderAtEnd(self.builder, calc_done_bb);
                    let total_size =
                        LLVMBuildAdd(self.builder, size_phi, one, c"total.size".as_ptr());
                    let result = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [total_size].as_mut_ptr(),
                        1,
                        c"result".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i8_ty, 0, 0), result);

                    // Build result string
                    let build_entry = LLVMGetInsertBlock(self.builder);
                    let build_loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(build_entry),
                        c"build.loop".as_ptr(),
                    );
                    let build_done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(build_entry),
                        c"build.done".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, build_loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, build_loop_bb);

                    let build_idx_phi = LLVMBuildPhi(self.builder, i64_ty, c"build.idx".as_ptr());
                    LLVMAddIncoming(
                        build_idx_phi,
                        [zero].as_mut_ptr(),
                        [build_entry].as_mut_ptr(),
                        1,
                    );

                    let done_build = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                        build_idx_phi,
                        vec_len,
                        c"done.build".as_ptr(),
                    );

                    let build_body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"build.body".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, done_build, build_done_bb, build_body_bb);
                    LLVMPositionBuilderAtEnd(self.builder, build_body_bb);

                    // Concatenate string
                    let elem_ptr2 = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        data_ptr,
                        [build_idx_phi].as_mut_ptr(),
                        1,
                        c"elem".as_ptr(),
                    );
                    let str_at_idx2 =
                        LLVMBuildLoad2(self.builder, i8_ptr_ty, elem_ptr2, c"str".as_ptr());

                    let strcat_fn = *self
                        .functions
                        .get("strcat")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcat"))?;
                    let strcat_ty = LLVMGlobalGetValueType(strcat_fn);
                    LLVMBuildCall2(
                        self.builder,
                        strcat_ty,
                        strcat_fn,
                        [result, str_at_idx2].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    // Add separator if not last
                    let next_build_idx =
                        LLVMBuildAdd(self.builder, build_idx_phi, one, c"next.build.idx".as_ptr());
                    let is_last2 = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        next_build_idx,
                        vec_len,
                        c"is.last2".as_ptr(),
                    );

                    let add_sep_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"add.sep".as_ptr(),
                    );
                    let skip_sep_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"skip.sep".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, is_last2, skip_sep_bb, add_sep_bb);

                    LLVMPositionBuilderAtEnd(self.builder, add_sep_bb);
                    LLVMBuildCall2(
                        self.builder,
                        strcat_ty,
                        strcat_fn,
                        [result, sep].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, skip_sep_bb);

                    LLVMPositionBuilderAtEnd(self.builder, skip_sep_bb);
                    LLVMAddIncoming(
                        build_idx_phi,
                        [next_build_idx].as_mut_ptr(),
                        [skip_sep_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMBuildBr(self.builder, build_loop_bb);

                    LLVMPositionBuilderAtEnd(self.builder, build_done_bb);
                    LLVMBuildBr(self.builder, done_bb);

                    // Merge
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    let result_phi = LLVMBuildPhi(self.builder, i8_ptr_ty, c"result".as_ptr());
                    LLVMAddIncoming(
                        result_phi,
                        [empty_str].as_mut_ptr(),
                        [empty_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        result_phi,
                        [result].as_mut_ptr(),
                        [build_done_bb].as_mut_ptr(),
                        1,
                    );

                    Ok(Some(result_phi))
                }

                "str_replace" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    let old = self.codegen_expression(&arguments[1])?;
                    let new = self.codegen_expression(&arguments[2])?;

                    self.emit_null_check(s, "str_replace: null string")?;
                    self.emit_null_check(old, "str_replace: null old")?;
                    self.emit_null_check(new, "str_replace: null new")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    let strlen_fn = *self
                        .functions
                        .get("strlen")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strlen"))?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);

                    let old_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [old].as_mut_ptr(),
                        1,
                        c"old.len".as_ptr(),
                    );
                    let new_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [new].as_mut_ptr(),
                        1,
                        c"new.len".as_ptr(),
                    );

                    // If old is empty, return copy of original
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    let old_empty = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        old_len,
                        zero,
                        c"old.empty".as_ptr(),
                    );

                    let return_orig_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"return.orig".as_ptr(),
                    );
                    let do_replace_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"do.replace".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"done".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, old_empty, return_orig_bb, do_replace_bb);

                    // Return copy of original
                    LLVMPositionBuilderAtEnd(self.builder, return_orig_bb);
                    let s_len = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"s.len".as_ptr(),
                    );
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let copy_size = LLVMBuildAdd(self.builder, s_len, one, c"copy.size".as_ptr());

                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let orig_copy = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [copy_size].as_mut_ptr(),
                        1,
                        c"orig.copy".as_ptr(),
                    );

                    let strcpy_fn = *self
                        .functions
                        .get("strcpy")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcpy"))?;
                    let strcpy_ty = LLVMGlobalGetValueType(strcpy_fn);
                    LLVMBuildCall2(
                        self.builder,
                        strcpy_ty,
                        strcpy_fn,
                        [orig_copy, s].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, done_bb);

                    // Do replacement: allocate generous buffer and build result
                    LLVMPositionBuilderAtEnd(self.builder, do_replace_bb);
                    let s_len2 = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"s.len2".as_ptr(),
                    );

                    // Allocate buffer: s_len * 2 + new_len * 10 (generous estimate)
                    let buf_size = LLVMBuildMul(
                        self.builder,
                        s_len2,
                        LLVMConstInt(i64_ty, 2, 0),
                        c"".as_ptr(),
                    );
                    let new_contrib = LLVMBuildMul(
                        self.builder,
                        new_len,
                        LLVMConstInt(i64_ty, 10, 0),
                        c"".as_ptr(),
                    );
                    let total_buf = LLVMBuildAdd(self.builder, buf_size, new_contrib, c"".as_ptr());
                    let final_buf = LLVMBuildAdd(
                        self.builder,
                        total_buf,
                        LLVMConstInt(i64_ty, 100, 0),
                        c"final.buf".as_ptr(),
                    );

                    let result = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [final_buf].as_mut_ptr(),
                        1,
                        c"result".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i8_ty, 0, 0), result);

                    // Replace loop using strstr
                    let strstr_fn = *self
                        .functions
                        .get("strstr")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strstr"))?;
                    let strstr_ty = LLVMGlobalGetValueType(strstr_fn);
                    let strcat_fn = *self
                        .functions
                        .get("strcat")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcat"))?;
                    let strcat_ty = LLVMGlobalGetValueType(strcat_fn);
                    let strncat_fn = *self
                        .functions
                        .get("strncat")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strncat"))?;
                    let strncat_ty = LLVMGlobalGetValueType(strncat_fn);

                    let replace_entry = LLVMGetInsertBlock(self.builder);
                    let replace_loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(replace_entry),
                        c"replace.loop".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, replace_loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, replace_loop_bb);

                    let current_phi = LLVMBuildPhi(self.builder, i8_ptr_ty, c"current".as_ptr());
                    LLVMAddIncoming(
                        current_phi,
                        [s].as_mut_ptr(),
                        [replace_entry].as_mut_ptr(),
                        1,
                    );

                    // Find next occurrence
                    let found_ptr = LLVMBuildCall2(
                        self.builder,
                        strstr_ty,
                        strstr_fn,
                        [current_phi, old].as_mut_ptr(),
                        2,
                        c"found".as_ptr(),
                    );

                    let null_ptr = LLVMConstNull(i8_ptr_ty);
                    let found = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntNE,
                        found_ptr,
                        null_ptr,
                        c"found".as_ptr(),
                    );

                    let replace_part_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"replace.part".as_ptr(),
                    );
                    let replace_final_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder)),
                        c"replace.final".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, found, replace_part_bb, replace_final_bb);

                    // Replace part
                    LLVMPositionBuilderAtEnd(self.builder, replace_part_bb);
                    let current_int = LLVMBuildPtrToInt(
                        self.builder,
                        current_phi,
                        i64_ty,
                        c"current.int".as_ptr(),
                    );
                    let found_int =
                        LLVMBuildPtrToInt(self.builder, found_ptr, i64_ty, c"found.int".as_ptr());
                    let before_len =
                        LLVMBuildSub(self.builder, found_int, current_int, c"before.len".as_ptr());

                    // Append part before match
                    LLVMBuildCall2(
                        self.builder,
                        strncat_ty,
                        strncat_fn,
                        [result, current_phi, before_len].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    // Append replacement
                    LLVMBuildCall2(
                        self.builder,
                        strcat_ty,
                        strcat_fn,
                        [result, new].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    // Move past old string
                    let next_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ty,
                        found_ptr,
                        [old_len].as_mut_ptr(),
                        1,
                        c"next.ptr".as_ptr(),
                    );
                    LLVMAddIncoming(
                        current_phi,
                        [next_ptr].as_mut_ptr(),
                        [replace_part_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMBuildBr(self.builder, replace_loop_bb);

                    // Append final part
                    LLVMPositionBuilderAtEnd(self.builder, replace_final_bb);
                    LLVMBuildCall2(
                        self.builder,
                        strcat_ty,
                        strcat_fn,
                        [result, current_phi].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, done_bb);

                    // Merge
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    let result_phi = LLVMBuildPhi(self.builder, i8_ptr_ty, c"result".as_ptr());
                    LLVMAddIncoming(
                        result_phi,
                        [orig_copy].as_mut_ptr(),
                        [return_orig_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        result_phi,
                        [result].as_mut_ptr(),
                        [replace_final_bb].as_mut_ptr(),
                        1,
                    );

                    Ok(Some(result_phi))
                }

                // ============================================================
                // Math Stdlib: math_sqrt, math_pow, math_abs, etc.
                // ============================================================
                "math_sqrt" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let sqrt_fn = *self
                        .functions
                        .get("sqrt")
                        .ok_or_else(|| CompilerError::codegen_error("Missing sqrt"))?;
                    let sqrt_ty = LLVMGlobalGetValueType(sqrt_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        sqrt_ty,
                        sqrt_fn,
                        [x].as_mut_ptr(),
                        1,
                        c"math_sqrt".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "math_pow" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let y = self.codegen_expression(&arguments[1])?;
                    let pow_fn = *self
                        .functions
                        .get("pow")
                        .ok_or_else(|| CompilerError::codegen_error("Missing pow"))?;
                    let pow_ty = LLVMGlobalGetValueType(pow_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        pow_ty,
                        pow_fn,
                        [x, y].as_mut_ptr(),
                        2,
                        c"math_pow".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "math_abs" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let abs_fn = *self
                        .functions
                        .get("abs")
                        .ok_or_else(|| CompilerError::codegen_error("Missing abs"))?;
                    let abs_ty = LLVMGlobalGetValueType(abs_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        abs_ty,
                        abs_fn,
                        [x].as_mut_ptr(),
                        1,
                        c"math_abs".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "math_floor" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let floor_fn = *self
                        .functions
                        .get("floor")
                        .ok_or_else(|| CompilerError::codegen_error("Missing floor"))?;
                    let floor_ty = LLVMGlobalGetValueType(floor_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        floor_ty,
                        floor_fn,
                        [x].as_mut_ptr(),
                        1,
                        c"math_floor".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "math_ceil" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let ceil_fn = *self
                        .functions
                        .get("ceil")
                        .ok_or_else(|| CompilerError::codegen_error("Missing ceil"))?;
                    let ceil_ty = LLVMGlobalGetValueType(ceil_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        ceil_ty,
                        ceil_fn,
                        [x].as_mut_ptr(),
                        1,
                        c"math_ceil".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "math_round" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let round_fn = *self
                        .functions
                        .get("round")
                        .ok_or_else(|| CompilerError::codegen_error("Missing round"))?;
                    let round_ty = LLVMGlobalGetValueType(round_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        round_ty,
                        round_fn,
                        [x].as_mut_ptr(),
                        1,
                        c"math_round".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "math_sin" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let sin_fn = *self
                        .functions
                        .get("sin")
                        .ok_or_else(|| CompilerError::codegen_error("Missing sin"))?;
                    let sin_ty = LLVMGlobalGetValueType(sin_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        sin_ty,
                        sin_fn,
                        [x].as_mut_ptr(),
                        1,
                        c"math_sin".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "math_cos" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let cos_fn = *self
                        .functions
                        .get("cos")
                        .ok_or_else(|| CompilerError::codegen_error("Missing cos"))?;
                    let cos_ty = LLVMGlobalGetValueType(cos_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        cos_ty,
                        cos_fn,
                        [x].as_mut_ptr(),
                        1,
                        c"math_cos".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "math_tan" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let tan_fn = *self
                        .functions
                        .get("tan")
                        .ok_or_else(|| CompilerError::codegen_error("Missing tan"))?;
                    let tan_ty = LLVMGlobalGetValueType(tan_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        tan_ty,
                        tan_fn,
                        [x].as_mut_ptr(),
                        1,
                        c"math_tan".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "math_min" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;
                    // min(a, b) = a < b ? a : b
                    let cond = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                        a,
                        b,
                        c"min.cmp".as_ptr(),
                    );
                    let result = LLVMBuildSelect(self.builder, cond, a, b, c"math_min".as_ptr());
                    Ok(Some(result))
                }

                "math_max" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;
                    // max(a, b) = a > b ? a : b
                    let cond = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                        a,
                        b,
                        c"max.cmp".as_ptr(),
                    );
                    let result = LLVMBuildSelect(self.builder, cond, a, b, c"math_max".as_ptr());
                    Ok(Some(result))
                }

                // ============================================================
                // Random Stdlib: rand_int, rand_float, rand_seed
                // ============================================================
                "rand_seed" => {
                    let seed = self.codegen_expression(&arguments[0])?;
                    let srand_fn = *self
                        .functions
                        .get("srand")
                        .ok_or_else(|| CompilerError::codegen_error("Missing srand"))?;
                    let srand_ty = LLVMGlobalGetValueType(srand_fn);
                    LLVMBuildCall2(
                        self.builder,
                        srand_ty,
                        srand_fn,
                        [seed].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "rand_int" => {
                    let min = self.codegen_expression(&arguments[0])?;
                    let max = self.codegen_expression(&arguments[1])?;
                    let rand_fn = *self
                        .functions
                        .get("rand")
                        .ok_or_else(|| CompilerError::codegen_error("Missing rand"))?;
                    let rand_ty = LLVMGlobalGetValueType(rand_fn);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    // rand() returns int, convert to i64
                    let rand_val = LLVMBuildCall2(
                        self.builder,
                        rand_ty,
                        rand_fn,
                        [].as_mut_ptr(),
                        0,
                        c"rand".as_ptr(),
                    );
                    let rand_i64 =
                        LLVMBuildSExt(self.builder, rand_val, i64_ty, c"rand.ext".as_ptr());

                    // result = min + (rand % (max - min + 1))
                    let range = LLVMBuildSub(self.builder, max, min, c"range.sub".as_ptr());
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let range_plus_one =
                        LLVMBuildAdd(self.builder, range, one, c"range.add".as_ptr());
                    let mod_val =
                        LLVMBuildSRem(self.builder, rand_i64, range_plus_one, c"rand.mod".as_ptr());
                    let result = LLVMBuildAdd(self.builder, min, mod_val, c"rand_int".as_ptr());
                    Ok(Some(result))
                }

                "rand_float" => {
                    let rand_fn = *self
                        .functions
                        .get("rand")
                        .ok_or_else(|| CompilerError::codegen_error("Missing rand"))?;
                    let rand_ty = LLVMGlobalGetValueType(rand_fn);
                    let f64_ty = LLVMDoubleTypeInContext(self.context);

                    // rand() / RAND_MAX -> 0.0 to 1.0
                    let rand_val = LLVMBuildCall2(
                        self.builder,
                        rand_ty,
                        rand_fn,
                        [].as_mut_ptr(),
                        0,
                        c"rand".as_ptr(),
                    );
                    let rand_f64 =
                        LLVMBuildSIToFP(self.builder, rand_val, f64_ty, c"rand.fp".as_ptr());
                    let rand_max = LLVMConstReal(f64_ty, 2147483647.0); // RAND_MAX
                    let result =
                        LLVMBuildFDiv(self.builder, rand_f64, rand_max, c"rand_float".as_ptr());
                    Ok(Some(result))
                }

                "rand_bytes" => {
                    // Allocate n bytes and fill with random data
                    let n = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let _i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    // Allocate buffer
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let buf = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [n].as_mut_ptr(),
                        1,
                        c"rand_buf".as_ptr(),
                    );

                    // Get rand function
                    let rand_fn = *self
                        .functions
                        .get("rand")
                        .ok_or_else(|| CompilerError::codegen_error("Missing rand"))?;
                    let rand_ty = LLVMGlobalGetValueType(rand_fn);

                    // Create loop to fill buffer
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"rand.loop".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"rand.done".as_ptr(),
                    );

                    // Alloca for loop counter
                    let counter = LLVMBuildAlloca(self.builder, i64_ty, c"i".as_ptr());
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), counter);
                    LLVMBuildBr(self.builder, loop_bb);

                    // Loop block
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    let i = LLVMBuildLoad2(self.builder, i64_ty, counter, c"i.val".as_ptr());
                    let cond = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                        i,
                        n,
                        c"cond".as_ptr(),
                    );

                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"rand.body".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, cond, body_bb, done_bb);

                    // Body block
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    let rand_val = LLVMBuildCall2(
                        self.builder,
                        rand_ty,
                        rand_fn,
                        [].as_mut_ptr(),
                        0,
                        c"rand".as_ptr(),
                    );
                    let rand_byte = LLVMBuildTrunc(self.builder, rand_val, i8_ty, c"byte".as_ptr());
                    let ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ty,
                        buf,
                        [i].as_mut_ptr(),
                        1,
                        c"ptr".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, rand_byte, ptr);

                    // Increment counter
                    let next_i = LLVMBuildAdd(
                        self.builder,
                        i,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"next_i".as_ptr(),
                    );
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
                    let prefix_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        prefix.as_ptr(),
                        c"log.prefix".as_ptr(),
                    );

                    let printf_fn = *self
                        .functions
                        .get("printf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing printf"))?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("%s%s\n").expect("CString failed");
                    let fmt_ptr =
                        LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"log.fmt".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        printf_ty,
                        printf_fn,
                        [fmt_ptr, prefix_ptr, msg].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "log_info" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let prefix = CString::new("[INFO] ").expect("CString failed");
                    let prefix_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        prefix.as_ptr(),
                        c"log.prefix".as_ptr(),
                    );

                    let printf_fn = *self
                        .functions
                        .get("printf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing printf"))?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("%s%s\n").expect("CString failed");
                    let fmt_ptr =
                        LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"log.fmt".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        printf_ty,
                        printf_fn,
                        [fmt_ptr, prefix_ptr, msg].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "log_warn" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let prefix = CString::new("[WARN] ").expect("CString failed");
                    let prefix_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        prefix.as_ptr(),
                        c"log.prefix".as_ptr(),
                    );

                    let printf_fn = *self
                        .functions
                        .get("printf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing printf"))?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("%s%s\n").expect("CString failed");
                    let fmt_ptr =
                        LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"log.fmt".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        printf_ty,
                        printf_fn,
                        [fmt_ptr, prefix_ptr, msg].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "log_error" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let prefix = CString::new("[ERROR] ").expect("CString failed");
                    let prefix_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        prefix.as_ptr(),
                        c"log.prefix".as_ptr(),
                    );

                    let printf_fn = *self
                        .functions
                        .get("printf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing printf"))?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("%s%s\n").expect("CString failed");
                    let fmt_ptr =
                        LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"log.fmt".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        printf_ty,
                        printf_fn,
                        [fmt_ptr, prefix_ptr, msg].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

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
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let buf_size = LLVMConstInt(i64_ty, 32, 0);
                    let buf = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [buf_size].as_mut_ptr(),
                        1,
                        c"fmt.buf".as_ptr(),
                    );

                    let sprintf_fn = *self
                        .functions
                        .get("sprintf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing sprintf"))?;
                    let sprintf_ty = LLVMGlobalGetValueType(sprintf_fn);
                    let fmt = CString::new("%ld").expect("CString failed");
                    let fmt_ptr =
                        LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"fmt.int".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        sprintf_ty,
                        sprintf_fn,
                        [buf, fmt_ptr, n].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    Ok(Some(buf))
                }

                "fmt_hex" => {
                    let n = self.codegen_expression(&arguments[0])?;

                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let buf_size = LLVMConstInt(i64_ty, 32, 0);
                    let buf = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [buf_size].as_mut_ptr(),
                        1,
                        c"fmt.buf".as_ptr(),
                    );

                    let sprintf_fn = *self
                        .functions
                        .get("sprintf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing sprintf"))?;
                    let sprintf_ty = LLVMGlobalGetValueType(sprintf_fn);
                    let fmt = CString::new("0x%lx").expect("CString failed");
                    let fmt_ptr =
                        LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"fmt.hex".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        sprintf_ty,
                        sprintf_fn,
                        [buf, fmt_ptr, n].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    Ok(Some(buf))
                }

                "fmt_bool" => {
                    let b = self.codegen_expression(&arguments[0])?;
                    let _i1_ty = LLVMInt1TypeInContext(self.context);

                    // Convert to i1 if needed
                    let cond = if LLVMGetTypeKind(LLVMTypeOf(b))
                        == llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind
                        && LLVMGetIntTypeWidth(LLVMTypeOf(b)) != 1
                    {
                        let zero = LLVMConstInt(LLVMTypeOf(b), 0, 0);
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntNE,
                            b,
                            zero,
                            c"fmt.cond".as_ptr(),
                        )
                    } else {
                        b
                    };

                    let true_str = CString::new("true").expect("CString failed");
                    let false_str = CString::new("false").expect("CString failed");
                    let true_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        true_str.as_ptr(),
                        c"fmt.true".as_ptr(),
                    );
                    let false_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        false_str.as_ptr(),
                        c"fmt.false".as_ptr(),
                    );

                    let result = LLVMBuildSelect(
                        self.builder,
                        cond,
                        true_ptr,
                        false_ptr,
                        c"fmt_bool".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "fmt_float" => {
                    let f = self.codegen_expression(&arguments[0])?;
                    let precision = self.codegen_expression(&arguments[1])?;

                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let buf_size = LLVMConstInt(i64_ty, 64, 0);
                    let buf = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [buf_size].as_mut_ptr(),
                        1,
                        c"fmt.buf".as_ptr(),
                    );

                    let sprintf_fn = *self
                        .functions
                        .get("sprintf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing sprintf"))?;
                    let sprintf_ty = LLVMGlobalGetValueType(sprintf_fn);
                    let fmt = CString::new("%.*f").expect("CString failed");
                    let fmt_ptr =
                        LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"fmt.float".as_ptr());

                    // Truncate precision to i32 for printf
                    let i32_ty = LLVMInt32TypeInContext(self.context);
                    let prec_i32 =
                        LLVMBuildTrunc(self.builder, precision, i32_ty, c"prec.trunc".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        sprintf_ty,
                        sprintf_fn,
                        [buf, fmt_ptr, prec_i32, f].as_mut_ptr(),
                        4,
                        c"".as_ptr(),
                    );

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
                    let cond_i1 = if LLVMGetTypeKind(LLVMTypeOf(cond))
                        == llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind
                        && LLVMGetIntTypeWidth(LLVMTypeOf(cond)) != 1
                    {
                        let zero = LLVMConstInt(LLVMTypeOf(cond), 0, 0);
                        LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntNE,
                            cond,
                            zero,
                            c"assert.cond".as_ptr(),
                        )
                    } else {
                        cond
                    };

                    // Create blocks for pass/fail
                    let current_fn = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("assert: no current function")
                    })?;
                    let pass_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"assert.pass".as_ptr(),
                    );
                    let fail_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"assert.fail".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, cond_i1, pass_bb, fail_bb);

                    // Fail block: print message and abort
                    LLVMPositionBuilderAtEnd(self.builder, fail_bb);
                    let msg_str = CString::new("Assertion failed").expect("CString failed");
                    let msg = LLVMBuildGlobalStringPtr(
                        self.builder,
                        msg_str.as_ptr(),
                        c"assert.msg".as_ptr(),
                    );
                    let puts_fn = *self
                        .functions
                        .get("puts")
                        .ok_or_else(|| CompilerError::codegen_error("Missing puts"))?;
                    let puts_ty = LLVMGlobalGetValueType(puts_fn);
                    LLVMBuildCall2(
                        self.builder,
                        puts_ty,
                        puts_fn,
                        [msg].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    let abort_fn = *self
                        .functions
                        .get("abort")
                        .ok_or_else(|| CompilerError::codegen_error("Missing abort"))?;
                    let abort_ty = LLVMGlobalGetValueType(abort_fn);
                    LLVMBuildCall2(
                        self.builder,
                        abort_ty,
                        abort_fn,
                        [].as_mut_ptr(),
                        0,
                        c"".as_ptr(),
                    );
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
                    let cond = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        a,
                        b,
                        c"assert_eq.cmp".as_ptr(),
                    );

                    // Create blocks for pass/fail
                    let current_fn = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("assert_eq: no current function")
                    })?;
                    let pass_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"assert_eq.pass".as_ptr(),
                    );
                    let fail_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"assert_eq.fail".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, cond, pass_bb, fail_bb);

                    // Fail block: print message and abort
                    LLVMPositionBuilderAtEnd(self.builder, fail_bb);
                    let msg_str =
                        CString::new("Assertion failed: values not equal").expect("CString failed");
                    let msg = LLVMBuildGlobalStringPtr(
                        self.builder,
                        msg_str.as_ptr(),
                        c"assert_eq.msg".as_ptr(),
                    );
                    let puts_fn = *self
                        .functions
                        .get("puts")
                        .ok_or_else(|| CompilerError::codegen_error("Missing puts"))?;
                    let puts_ty = LLVMGlobalGetValueType(puts_fn);
                    LLVMBuildCall2(
                        self.builder,
                        puts_ty,
                        puts_fn,
                        [msg].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    let abort_fn = *self
                        .functions
                        .get("abort")
                        .ok_or_else(|| CompilerError::codegen_error("Missing abort"))?;
                    let abort_ty = LLVMGlobalGetValueType(abort_fn);
                    LLVMBuildCall2(
                        self.builder,
                        abort_ty,
                        abort_fn,
                        [].as_mut_ptr(),
                        0,
                        c"".as_ptr(),
                    );
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
                    let cond = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntNE,
                        a,
                        b,
                        c"assert_ne.cmp".as_ptr(),
                    );

                    // Create blocks for pass/fail
                    let current_fn = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("assert_ne: no current function")
                    })?;
                    let pass_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"assert_ne.pass".as_ptr(),
                    );
                    let fail_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"assert_ne.fail".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, cond, pass_bb, fail_bb);

                    // Fail block: print message and abort
                    LLVMPositionBuilderAtEnd(self.builder, fail_bb);
                    let msg_str =
                        CString::new("Assertion failed: values are equal").expect("CString failed");
                    let msg = LLVMBuildGlobalStringPtr(
                        self.builder,
                        msg_str.as_ptr(),
                        c"assert_ne.msg".as_ptr(),
                    );
                    let puts_fn = *self
                        .functions
                        .get("puts")
                        .ok_or_else(|| CompilerError::codegen_error("Missing puts"))?;
                    let puts_ty = LLVMGlobalGetValueType(puts_fn);
                    LLVMBuildCall2(
                        self.builder,
                        puts_ty,
                        puts_fn,
                        [msg].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    let abort_fn = *self
                        .functions
                        .get("abort")
                        .ok_or_else(|| CompilerError::codegen_error("Missing abort"))?;
                    let abort_ty = LLVMGlobalGetValueType(abort_fn);
                    LLVMBuildCall2(
                        self.builder,
                        abort_ty,
                        abort_fn,
                        [].as_mut_ptr(),
                        0,
                        c"".as_ptr(),
                    );
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
                    let printf_fn = *self
                        .functions
                        .get("printf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing printf"))?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("\x1b[32m[PASS]\x1b[0m %s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        fmt.as_ptr(),
                        c"test.pass.fmt".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        printf_ty,
                        printf_fn,
                        [fmt_ptr, msg].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "test_fail" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let printf_fn = *self
                        .functions
                        .get("printf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing printf"))?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("\x1b[31m[FAIL]\x1b[0m %s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        fmt.as_ptr(),
                        c"test.fail.fmt".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        printf_ty,
                        printf_fn,
                        [fmt_ptr, msg].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let abort_fn = *self
                        .functions
                        .get("abort")
                        .ok_or_else(|| CompilerError::codegen_error("Missing abort"))?;
                    let abort_ty = LLVMGlobalGetValueType(abort_fn);
                    LLVMBuildCall2(
                        self.builder,
                        abort_ty,
                        abort_fn,
                        [].as_mut_ptr(),
                        0,
                        c"".as_ptr(),
                    );
                    LLVMBuildUnreachable(self.builder);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "test_skip" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let printf_fn = *self
                        .functions
                        .get("printf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing printf"))?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("\x1b[33m[SKIP]\x1b[0m %s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        fmt.as_ptr(),
                        c"test.skip.fmt".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        printf_ty,
                        printf_fn,
                        [fmt_ptr, msg].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "test_section" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let printf_fn = *self
                        .functions
                        .get("printf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing printf"))?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("\n\x1b[1m=== %s ===\x1b[0m\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(
                        self.builder,
                        fmt.as_ptr(),
                        c"test.section.fmt".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        printf_ty,
                        printf_fn,
                        [fmt_ptr, msg].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
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
                    let printf_fn = *self
                        .functions
                        .get("printf")
                        .ok_or_else(|| CompilerError::codegen_error("Missing printf"))?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt =
                        CString::new("\x1b[36m[BENCH]\x1b[0m %s: completed (%ld iterations)\n")
                            .expect("CString failed");
                    let fmt_ptr =
                        LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"bench.fmt".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        printf_ty,
                        printf_fn,
                        [fmt_ptr, name, iterations].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                // ============================================================
                // Threading Primitives
                // ============================================================
                "thread_sleep_ms" => {
                    // Delegate to existing sleep_ms implementation
                    let ms = self.codegen_expression(&arguments[0])?;

                    // Convert milliseconds to microseconds (usleep takes microseconds)
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let thousand = LLVMConstInt(i64_ty, 1000, 0);
                    let us = LLVMBuildMul(self.builder, ms, thousand, c"us".as_ptr());

                    // Call usleep(microseconds)
                    let usleep_fn = *self
                        .functions
                        .get("usleep")
                        .ok_or_else(|| CompilerError::codegen_error("Missing usleep"))?;
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

                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                // ============================================================
                // Thread Primitives
                // ============================================================
                "thread_spawn" => {
                    // thread_spawn takes a function pointer and spawns a new thread
                    let func_ptr = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);

                    // Allocate space for pthread_t (8 bytes on 64-bit)
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let thread_size = LLVMConstInt(i64_ty, 8, 0);
                    let thread_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [thread_size].as_mut_ptr(),
                        1,
                        c"thread.ptr".as_ptr(),
                    );

                    // Convert function pointer from int to actual pointer
                    let func_as_ptr =
                        LLVMBuildIntToPtr(self.builder, func_ptr, i8_ptr_ty, c"func.ptr".as_ptr());

                    // Call pthread_create(thread_ptr, NULL, func, NULL)
                    let null_ptr = LLVMConstNull(i8_ptr_ty);
                    let pthread_create_fn = *self
                        .functions
                        .get("pthread_create")
                        .ok_or_else(|| CompilerError::codegen_error("Missing pthread_create"))?;
                    let pthread_create_ty = LLVMGlobalGetValueType(pthread_create_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_create_ty,
                        pthread_create_fn,
                        [thread_ptr, null_ptr, func_as_ptr, null_ptr].as_mut_ptr(),
                        4,
                        c"".as_ptr(),
                    );

                    // Return thread handle as int
                    let result = LLVMBuildPtrToInt(
                        self.builder,
                        thread_ptr,
                        i64_ty,
                        c"thread.handle".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "thread_join" => {
                    let thread_handle = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    // Convert handle back to pointer to pthread_t
                    let thread_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        thread_handle,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"thread.ptr".as_ptr(),
                    );

                    // Load the pthread_t value (which is a pointer on macOS)
                    let thread_id =
                        LLVMBuildLoad2(self.builder, i8_ptr_ty, thread_ptr, c"thread.id".as_ptr());

                    // Call pthread_join(thread_id, NULL)
                    let null_ptr = LLVMConstNull(i8_ptr_ty);
                    let pthread_join_fn = *self
                        .functions
                        .get("pthread_join")
                        .ok_or_else(|| CompilerError::codegen_error("Missing pthread_join"))?;
                    let pthread_join_ty = LLVMGlobalGetValueType(pthread_join_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_join_ty,
                        pthread_join_fn,
                        [thread_id, null_ptr].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    // Free the thread handle memory
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    let thread_ptr_i8 =
                        LLVMBuildBitCast(self.builder, thread_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [thread_ptr_i8].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    // Return 0 for now (could return thread result)
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "thread_detach" => {
                    let thread_handle = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);

                    // Convert handle back to pointer
                    let thread_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        thread_handle,
                        LLVMPointerType(i64_ty, 0),
                        c"thread.ptr".as_ptr(),
                    );

                    // Load the pthread_t value
                    let thread_id =
                        LLVMBuildLoad2(self.builder, i64_ty, thread_ptr, c"thread.id".as_ptr());

                    // Call pthread_detach(thread_id)
                    let pthread_detach_fn = *self
                        .functions
                        .get("pthread_detach")
                        .ok_or_else(|| CompilerError::codegen_error("Missing pthread_detach"))?;
                    let pthread_detach_ty = LLVMGlobalGetValueType(pthread_detach_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_detach_ty,
                        pthread_detach_fn,
                        [thread_id].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    // Free the thread handle memory
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    let thread_ptr_i8 =
                        LLVMBuildBitCast(self.builder, thread_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [thread_ptr_i8].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                // ============================================================
                // Mutex Primitives
                // ============================================================
                "mutex_create" => {
                    // Allocate space for pthread_mutex_t (64 bytes should be enough for most platforms)
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let mutex_size = LLVMConstInt(i64_ty, 64, 0);
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let mutex_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [mutex_size].as_mut_ptr(),
                        1,
                        c"mutex.ptr".as_ptr(),
                    );

                    // Initialize the mutex
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);
                    let null_ptr = LLVMConstNull(i8_ptr_ty);
                    let pthread_mutex_init_fn =
                        *self.functions.get("pthread_mutex_init").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_init")
                        })?;
                    let pthread_mutex_init_ty = LLVMGlobalGetValueType(pthread_mutex_init_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_init_ty,
                        pthread_mutex_init_fn,
                        [mutex_ptr, null_ptr].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    // Return mutex pointer as int
                    let result = LLVMBuildPtrToInt(
                        self.builder,
                        mutex_ptr,
                        i64_ty,
                        c"mutex.handle".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "mutex_lock" => {
                    let handle = self.codegen_expression(&arguments[0])?;
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let mutex_ptr =
                        LLVMBuildIntToPtr(self.builder, handle, i8_ptr_ty, c"mutex.ptr".as_ptr());
                    let pthread_mutex_lock_fn =
                        *self.functions.get("pthread_mutex_lock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_lock")
                        })?;
                    let pthread_mutex_lock_ty = LLVMGlobalGetValueType(pthread_mutex_lock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_lock_ty,
                        pthread_mutex_lock_fn,
                        [mutex_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "mutex_unlock" => {
                    let handle = self.codegen_expression(&arguments[0])?;
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let mutex_ptr =
                        LLVMBuildIntToPtr(self.builder, handle, i8_ptr_ty, c"mutex.ptr".as_ptr());
                    let pthread_mutex_unlock_fn =
                        *self.functions.get("pthread_mutex_unlock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_unlock")
                        })?;
                    let pthread_mutex_unlock_ty = LLVMGlobalGetValueType(pthread_mutex_unlock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_unlock_ty,
                        pthread_mutex_unlock_fn,
                        [mutex_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "mutex_destroy" => {
                    let handle = self.codegen_expression(&arguments[0])?;
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let mutex_ptr =
                        LLVMBuildIntToPtr(self.builder, handle, i8_ptr_ty, c"mutex.ptr".as_ptr());
                    let pthread_mutex_destroy_fn =
                        *self.functions.get("pthread_mutex_destroy").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_destroy")
                        })?;
                    let pthread_mutex_destroy_ty = LLVMGlobalGetValueType(pthread_mutex_destroy_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_destroy_ty,
                        pthread_mutex_destroy_fn,
                        [mutex_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    // Free the mutex memory
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [mutex_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                // ============================================================
                // Condition Variable Primitives
                // ============================================================
                "condvar_create" => {
                    // Allocate space for pthread_cond_t (64 bytes should be enough)
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let cond_size = LLVMConstInt(i64_ty, 64, 0);
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let cond_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [cond_size].as_mut_ptr(),
                        1,
                        c"condvar.ptr".as_ptr(),
                    );

                    // Initialize the condition variable
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);
                    let null_ptr = LLVMConstNull(i8_ptr_ty);
                    let pthread_cond_init_fn = *self
                        .functions
                        .get("pthread_cond_init")
                        .ok_or_else(|| CompilerError::codegen_error("Missing pthread_cond_init"))?;
                    let pthread_cond_init_ty = LLVMGlobalGetValueType(pthread_cond_init_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_init_ty,
                        pthread_cond_init_fn,
                        [cond_ptr, null_ptr].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    // Return condvar pointer as int
                    let result = LLVMBuildPtrToInt(
                        self.builder,
                        cond_ptr,
                        i64_ty,
                        c"condvar.handle".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "condvar_wait" => {
                    let cond_handle = self.codegen_expression(&arguments[0])?;
                    let mutex_handle = self.codegen_expression(&arguments[1])?;
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let cond_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        cond_handle,
                        i8_ptr_ty,
                        c"condvar.ptr".as_ptr(),
                    );
                    let mutex_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        mutex_handle,
                        i8_ptr_ty,
                        c"mutex.ptr".as_ptr(),
                    );
                    let pthread_cond_wait_fn = *self
                        .functions
                        .get("pthread_cond_wait")
                        .ok_or_else(|| CompilerError::codegen_error("Missing pthread_cond_wait"))?;
                    let pthread_cond_wait_ty = LLVMGlobalGetValueType(pthread_cond_wait_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_wait_ty,
                        pthread_cond_wait_fn,
                        [cond_ptr, mutex_ptr].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "condvar_signal" => {
                    let cond_handle = self.codegen_expression(&arguments[0])?;
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let cond_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        cond_handle,
                        i8_ptr_ty,
                        c"condvar.ptr".as_ptr(),
                    );
                    let pthread_cond_signal_fn =
                        *self.functions.get("pthread_cond_signal").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_cond_signal")
                        })?;
                    let pthread_cond_signal_ty = LLVMGlobalGetValueType(pthread_cond_signal_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_signal_ty,
                        pthread_cond_signal_fn,
                        [cond_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "condvar_broadcast" => {
                    let cond_handle = self.codegen_expression(&arguments[0])?;
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let cond_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        cond_handle,
                        i8_ptr_ty,
                        c"condvar.ptr".as_ptr(),
                    );
                    let pthread_cond_broadcast_fn = *self
                        .functions
                        .get("pthread_cond_broadcast")
                        .ok_or_else(|| {
                        CompilerError::codegen_error("Missing pthread_cond_broadcast")
                    })?;
                    let pthread_cond_broadcast_ty =
                        LLVMGlobalGetValueType(pthread_cond_broadcast_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_broadcast_ty,
                        pthread_cond_broadcast_fn,
                        [cond_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "condvar_destroy" => {
                    let cond_handle = self.codegen_expression(&arguments[0])?;
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let cond_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        cond_handle,
                        i8_ptr_ty,
                        c"condvar.ptr".as_ptr(),
                    );
                    let pthread_cond_destroy_fn =
                        *self.functions.get("pthread_cond_destroy").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_cond_destroy")
                        })?;
                    let pthread_cond_destroy_ty = LLVMGlobalGetValueType(pthread_cond_destroy_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_destroy_ty,
                        pthread_cond_destroy_fn,
                        [cond_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    // Free the condvar memory
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [cond_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                // ============================================================
                // Channel Primitives (mutex-protected ring buffer)
                // Layout: [mutex:64][condvar:64][buffer_ptr:8][capacity:8][head:8][tail:8][count:8][closed:8]
                // Total: 168 bytes
                // ============================================================
                "channel_create" => {
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    // Allocate channel struct (168 bytes)
                    let chan_size = LLVMConstInt(i64_ty, 168, 0);
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let chan_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [chan_size].as_mut_ptr(),
                        1,
                        c"channel.ptr".as_ptr(),
                    );

                    // Initialize mutex at offset 0
                    let null_ptr = LLVMConstNull(i8_ptr_ty);
                    let pthread_mutex_init_fn =
                        *self.functions.get("pthread_mutex_init").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_init")
                        })?;
                    let pthread_mutex_init_ty = LLVMGlobalGetValueType(pthread_mutex_init_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_init_ty,
                        pthread_mutex_init_fn,
                        [chan_ptr, null_ptr].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    // Initialize condvar at offset 64
                    let condvar_offset = LLVMConstInt(i64_ty, 64, 0);
                    let condvar_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [condvar_offset].as_mut_ptr(),
                        1,
                        c"condvar.ptr".as_ptr(),
                    );
                    let pthread_cond_init_fn = *self
                        .functions
                        .get("pthread_cond_init")
                        .ok_or_else(|| CompilerError::codegen_error("Missing pthread_cond_init"))?;
                    let pthread_cond_init_ty = LLVMGlobalGetValueType(pthread_cond_init_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_init_ty,
                        pthread_cond_init_fn,
                        [condvar_ptr, null_ptr].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );

                    // Allocate buffer (default capacity 16)
                    let capacity = LLVMConstInt(i64_ty, 16, 0);
                    let elem_size = LLVMConstInt(i64_ty, 8, 0);
                    let buf_size =
                        LLVMBuildMul(self.builder, capacity, elem_size, c"buf.size".as_ptr());
                    let buffer = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [buf_size].as_mut_ptr(),
                        1,
                        c"buffer".as_ptr(),
                    );

                    // Store buffer_ptr at offset 128
                    let buf_ptr_offset = LLVMConstInt(i64_ty, 128, 0);
                    let buf_ptr_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [buf_ptr_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let buf_ptr_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        buf_ptr_loc,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, buffer, buf_ptr_loc_typed);

                    // Store capacity at offset 136
                    let cap_offset = LLVMConstInt(i64_ty, 136, 0);
                    let cap_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [cap_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        cap_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, capacity, cap_loc_typed);

                    // Initialize head, tail, count, closed to 0
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    for offset in [144, 152, 160] {
                        let field_offset = LLVMConstInt(i64_ty, offset, 0);
                        let field_loc = LLVMBuildInBoundsGEP2(
                            self.builder,
                            i8_ty,
                            chan_ptr,
                            [field_offset].as_mut_ptr(),
                            1,
                            c"".as_ptr(),
                        );
                        let field_loc_typed = LLVMBuildBitCast(
                            self.builder,
                            field_loc,
                            LLVMPointerType(i64_ty, 0),
                            c"".as_ptr(),
                        );
                        LLVMBuildStore(self.builder, zero, field_loc_typed);
                    }

                    // Return channel handle
                    let result = LLVMBuildPtrToInt(
                        self.builder,
                        chan_ptr,
                        i64_ty,
                        c"channel.handle".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "channel_send" => {
                    // Blocking send with mutex synchronization
                    let chan_handle = self.codegen_expression(&arguments[0])?;
                    let value = self.codegen_expression(&arguments[1])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    let chan_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        chan_handle,
                        i8_ptr_ty,
                        c"chan.ptr".as_ptr(),
                    );

                    // Lock mutex
                    let pthread_mutex_lock_fn =
                        *self.functions.get("pthread_mutex_lock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_lock")
                        })?;
                    let pthread_mutex_lock_ty = LLVMGlobalGetValueType(pthread_mutex_lock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_lock_ty,
                        pthread_mutex_lock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    // Get buffer, tail, capacity, count
                    let buf_ptr_offset = LLVMConstInt(i64_ty, 128, 0);
                    let buf_ptr_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [buf_ptr_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let buf_ptr_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        buf_ptr_loc,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let buffer = LLVMBuildLoad2(
                        self.builder,
                        i8_ptr_ty,
                        buf_ptr_loc_typed,
                        c"buffer".as_ptr(),
                    );

                    let tail_offset = LLVMConstInt(i64_ty, 152, 0);
                    let tail_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [tail_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let tail_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        tail_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let tail =
                        LLVMBuildLoad2(self.builder, i64_ty, tail_loc_typed, c"tail".as_ptr());

                    let cap_offset = LLVMConstInt(i64_ty, 136, 0);
                    let cap_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [cap_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        cap_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let capacity =
                        LLVMBuildLoad2(self.builder, i64_ty, cap_loc_typed, c"capacity".as_ptr());

                    let count_offset = LLVMConstInt(i64_ty, 160, 0);
                    let count_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [count_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let count_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        count_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let count =
                        LLVMBuildLoad2(self.builder, i64_ty, count_loc_typed, c"count".as_ptr());

                    // Store value at buffer[tail]
                    let elem_size = LLVMConstInt(i64_ty, 8, 0);
                    let byte_offset = LLVMBuildMul(self.builder, tail, elem_size, c"".as_ptr());
                    let elem_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        buffer,
                        [byte_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let elem_ptr_typed = LLVMBuildBitCast(
                        self.builder,
                        elem_ptr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, elem_ptr_typed);

                    // Update tail = (tail + 1) % capacity
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let new_tail = LLVMBuildAdd(self.builder, tail, one, c"".as_ptr());
                    let new_tail = LLVMBuildURem(self.builder, new_tail, capacity, c"".as_ptr());
                    LLVMBuildStore(self.builder, new_tail, tail_loc_typed);

                    // Update count++
                    let new_count = LLVMBuildAdd(self.builder, count, one, c"".as_ptr());
                    LLVMBuildStore(self.builder, new_count, count_loc_typed);

                    // Signal condvar (wake up any waiting receivers)
                    let condvar_offset = LLVMConstInt(i64_ty, 64, 0);
                    let condvar_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [condvar_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let pthread_cond_signal_fn =
                        *self.functions.get("pthread_cond_signal").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_cond_signal")
                        })?;
                    let pthread_cond_signal_ty = LLVMGlobalGetValueType(pthread_cond_signal_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_signal_ty,
                        pthread_cond_signal_fn,
                        [condvar_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    // Unlock mutex
                    let pthread_mutex_unlock_fn =
                        *self.functions.get("pthread_mutex_unlock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_unlock")
                        })?;
                    let pthread_mutex_unlock_ty = LLVMGlobalGetValueType(pthread_mutex_unlock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_unlock_ty,
                        pthread_mutex_unlock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    let _ = cap_loc_typed; // suppress warning
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "channel_recv" => {
                    let chan_handle = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    let chan_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        chan_handle,
                        i8_ptr_ty,
                        c"chan.ptr".as_ptr(),
                    );

                    // Lock mutex
                    let pthread_mutex_lock_fn =
                        *self.functions.get("pthread_mutex_lock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_lock")
                        })?;
                    let pthread_mutex_lock_ty = LLVMGlobalGetValueType(pthread_mutex_lock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_lock_ty,
                        pthread_mutex_lock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    // Wait loop while count == 0
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let wait_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"chan.wait".as_ptr(),
                    );
                    let recv_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"chan.recv".as_ptr(),
                    );

                    LLVMBuildBr(self.builder, wait_bb);
                    LLVMPositionBuilderAtEnd(self.builder, wait_bb);

                    let count_offset = LLVMConstInt(i64_ty, 160, 0);
                    let count_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [count_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let count_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        count_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let count =
                        LLVMBuildLoad2(self.builder, i64_ty, count_loc_typed, c"count".as_ptr());

                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    let is_empty = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        count,
                        zero,
                        c"is.empty".as_ptr(),
                    );

                    let wait_cond_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"chan.wait.cond".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, is_empty, wait_cond_bb, recv_bb);

                    LLVMPositionBuilderAtEnd(self.builder, wait_cond_bb);
                    let condvar_offset = LLVMConstInt(i64_ty, 64, 0);
                    let condvar_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [condvar_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let pthread_cond_wait_fn = *self
                        .functions
                        .get("pthread_cond_wait")
                        .ok_or_else(|| CompilerError::codegen_error("Missing pthread_cond_wait"))?;
                    let pthread_cond_wait_ty = LLVMGlobalGetValueType(pthread_cond_wait_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_wait_ty,
                        pthread_cond_wait_fn,
                        [condvar_ptr, chan_ptr].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, wait_bb);

                    // Receive value
                    LLVMPositionBuilderAtEnd(self.builder, recv_bb);

                    let buf_ptr_offset = LLVMConstInt(i64_ty, 128, 0);
                    let buf_ptr_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [buf_ptr_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let buf_ptr_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        buf_ptr_loc,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let buffer = LLVMBuildLoad2(
                        self.builder,
                        i8_ptr_ty,
                        buf_ptr_loc_typed,
                        c"buffer".as_ptr(),
                    );

                    let head_offset = LLVMConstInt(i64_ty, 144, 0);
                    let head_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [head_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let head_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        head_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let head =
                        LLVMBuildLoad2(self.builder, i64_ty, head_loc_typed, c"head".as_ptr());

                    let cap_offset = LLVMConstInt(i64_ty, 136, 0);
                    let cap_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [cap_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        cap_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let capacity =
                        LLVMBuildLoad2(self.builder, i64_ty, cap_loc_typed, c"capacity".as_ptr());

                    // Read value from buffer[head]
                    let elem_size = LLVMConstInt(i64_ty, 8, 0);
                    let byte_offset = LLVMBuildMul(self.builder, head, elem_size, c"".as_ptr());
                    let elem_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        buffer,
                        [byte_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let elem_ptr_typed = LLVMBuildBitCast(
                        self.builder,
                        elem_ptr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let value =
                        LLVMBuildLoad2(self.builder, i64_ty, elem_ptr_typed, c"value".as_ptr());

                    // Update head = (head + 1) % capacity
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let new_head = LLVMBuildAdd(self.builder, head, one, c"".as_ptr());
                    let new_head =
                        LLVMBuildURem(self.builder, new_head, capacity, c"new.head".as_ptr());
                    LLVMBuildStore(self.builder, new_head, head_loc_typed);

                    // Update count--
                    let count2 =
                        LLVMBuildLoad2(self.builder, i64_ty, count_loc_typed, c"count2".as_ptr());
                    let new_count = LLVMBuildSub(self.builder, count2, one, c"new.count".as_ptr());
                    LLVMBuildStore(self.builder, new_count, count_loc_typed);

                    // Unlock mutex
                    let pthread_mutex_unlock_fn =
                        *self.functions.get("pthread_mutex_unlock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_unlock")
                        })?;
                    let pthread_mutex_unlock_ty = LLVMGlobalGetValueType(pthread_mutex_unlock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_unlock_ty,
                        pthread_mutex_unlock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    Ok(Some(value))
                }

                "channel_try_send" => {
                    // Non-blocking send - returns true if sent, false if full
                    let chan_handle = self.codegen_expression(&arguments[0])?;
                    let value = self.codegen_expression(&arguments[1])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i1_ty = LLVMInt1TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    let chan_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        chan_handle,
                        i8_ptr_ty,
                        c"chan.ptr".as_ptr(),
                    );

                    // Lock mutex
                    let pthread_mutex_lock_fn =
                        *self.functions.get("pthread_mutex_lock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_lock")
                        })?;
                    let pthread_mutex_lock_ty = LLVMGlobalGetValueType(pthread_mutex_lock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_lock_ty,
                        pthread_mutex_lock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    // Check if full
                    let count_offset = LLVMConstInt(i64_ty, 160, 0);
                    let count_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [count_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let count_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        count_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let count =
                        LLVMBuildLoad2(self.builder, i64_ty, count_loc_typed, c"count".as_ptr());

                    let cap_offset = LLVMConstInt(i64_ty, 136, 0);
                    let cap_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [cap_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        cap_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let capacity =
                        LLVMBuildLoad2(self.builder, i64_ty, cap_loc_typed, c"capacity".as_ptr());

                    let is_full = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        count,
                        capacity,
                        c"is.full".as_ptr(),
                    );

                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let send_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"try.send".as_ptr(),
                    );
                    let full_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"try.full".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"try.done".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, is_full, full_bb, send_bb);

                    // Send block
                    LLVMPositionBuilderAtEnd(self.builder, send_bb);
                    let buf_ptr_offset = LLVMConstInt(i64_ty, 128, 0);
                    let buf_ptr_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [buf_ptr_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let buf_ptr_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        buf_ptr_loc,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let buffer = LLVMBuildLoad2(
                        self.builder,
                        i8_ptr_ty,
                        buf_ptr_loc_typed,
                        c"buffer".as_ptr(),
                    );

                    let tail_offset = LLVMConstInt(i64_ty, 152, 0);
                    let tail_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [tail_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let tail_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        tail_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let tail =
                        LLVMBuildLoad2(self.builder, i64_ty, tail_loc_typed, c"tail".as_ptr());

                    let elem_size = LLVMConstInt(i64_ty, 8, 0);
                    let byte_offset = LLVMBuildMul(self.builder, tail, elem_size, c"".as_ptr());
                    let elem_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        buffer,
                        [byte_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let elem_ptr_typed = LLVMBuildBitCast(
                        self.builder,
                        elem_ptr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, elem_ptr_typed);

                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let new_tail = LLVMBuildAdd(self.builder, tail, one, c"".as_ptr());
                    let new_tail = LLVMBuildURem(self.builder, new_tail, capacity, c"".as_ptr());
                    LLVMBuildStore(self.builder, new_tail, tail_loc_typed);

                    let new_count = LLVMBuildAdd(self.builder, count, one, c"".as_ptr());
                    LLVMBuildStore(self.builder, new_count, count_loc_typed);

                    let condvar_offset = LLVMConstInt(i64_ty, 64, 0);
                    let condvar_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [condvar_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let pthread_cond_signal_fn =
                        *self.functions.get("pthread_cond_signal").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_cond_signal")
                        })?;
                    let pthread_cond_signal_ty = LLVMGlobalGetValueType(pthread_cond_signal_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_signal_ty,
                        pthread_cond_signal_fn,
                        [condvar_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    let pthread_mutex_unlock_fn =
                        *self.functions.get("pthread_mutex_unlock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_unlock")
                        })?;
                    let pthread_mutex_unlock_ty = LLVMGlobalGetValueType(pthread_mutex_unlock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_unlock_ty,
                        pthread_mutex_unlock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, done_bb);

                    // Full block
                    LLVMPositionBuilderAtEnd(self.builder, full_bb);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_unlock_ty,
                        pthread_mutex_unlock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, done_bb);

                    // Done block with phi
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    let phi = LLVMBuildPhi(self.builder, i1_ty, c"result".as_ptr());
                    let true_val = LLVMConstInt(i1_ty, 1, 0);
                    let false_val = LLVMConstInt(i1_ty, 0, 0);
                    LLVMAddIncoming(phi, [true_val].as_mut_ptr(), [send_bb].as_mut_ptr(), 1);
                    LLVMAddIncoming(phi, [false_val].as_mut_ptr(), [full_bb].as_mut_ptr(), 1);

                    Ok(Some(phi))
                }

                "channel_try_recv" => {
                    // Non-blocking recv - returns value or 0 if empty
                    let chan_handle = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    let chan_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        chan_handle,
                        i8_ptr_ty,
                        c"chan.ptr".as_ptr(),
                    );

                    // Lock mutex
                    let pthread_mutex_lock_fn =
                        *self.functions.get("pthread_mutex_lock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_lock")
                        })?;
                    let pthread_mutex_lock_ty = LLVMGlobalGetValueType(pthread_mutex_lock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_lock_ty,
                        pthread_mutex_lock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    let count_offset = LLVMConstInt(i64_ty, 160, 0);
                    let count_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [count_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let count_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        count_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let count =
                        LLVMBuildLoad2(self.builder, i64_ty, count_loc_typed, c"count".as_ptr());

                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    let is_empty = LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        count,
                        zero,
                        c"is.empty".as_ptr(),
                    );

                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let recv_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"try.recv".as_ptr(),
                    );
                    let empty_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"try.empty".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"try.done".as_ptr(),
                    );

                    LLVMBuildCondBr(self.builder, is_empty, empty_bb, recv_bb);

                    // Recv block
                    LLVMPositionBuilderAtEnd(self.builder, recv_bb);
                    let buf_ptr_offset = LLVMConstInt(i64_ty, 128, 0);
                    let buf_ptr_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [buf_ptr_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let buf_ptr_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        buf_ptr_loc,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let buffer = LLVMBuildLoad2(
                        self.builder,
                        i8_ptr_ty,
                        buf_ptr_loc_typed,
                        c"buffer".as_ptr(),
                    );

                    let head_offset = LLVMConstInt(i64_ty, 144, 0);
                    let head_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [head_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let head_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        head_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let head =
                        LLVMBuildLoad2(self.builder, i64_ty, head_loc_typed, c"head".as_ptr());

                    let cap_offset = LLVMConstInt(i64_ty, 136, 0);
                    let cap_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [cap_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        cap_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let capacity =
                        LLVMBuildLoad2(self.builder, i64_ty, cap_loc_typed, c"capacity".as_ptr());

                    let elem_size = LLVMConstInt(i64_ty, 8, 0);
                    let byte_offset = LLVMBuildMul(self.builder, head, elem_size, c"".as_ptr());
                    let elem_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        buffer,
                        [byte_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let elem_ptr_typed = LLVMBuildBitCast(
                        self.builder,
                        elem_ptr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let value =
                        LLVMBuildLoad2(self.builder, i64_ty, elem_ptr_typed, c"value".as_ptr());

                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let new_head = LLVMBuildAdd(self.builder, head, one, c"".as_ptr());
                    let new_head = LLVMBuildURem(self.builder, new_head, capacity, c"".as_ptr());
                    LLVMBuildStore(self.builder, new_head, head_loc_typed);

                    let new_count = LLVMBuildSub(self.builder, count, one, c"".as_ptr());
                    LLVMBuildStore(self.builder, new_count, count_loc_typed);

                    let pthread_mutex_unlock_fn =
                        *self.functions.get("pthread_mutex_unlock").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_unlock")
                        })?;
                    let pthread_mutex_unlock_ty = LLVMGlobalGetValueType(pthread_mutex_unlock_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_unlock_ty,
                        pthread_mutex_unlock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, done_bb);

                    // Empty block
                    LLVMPositionBuilderAtEnd(self.builder, empty_bb);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_unlock_ty,
                        pthread_mutex_unlock_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildBr(self.builder, done_bb);

                    // Done block with phi
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    let phi = LLVMBuildPhi(self.builder, i64_ty, c"result".as_ptr());
                    LLVMAddIncoming(phi, [value].as_mut_ptr(), [recv_bb].as_mut_ptr(), 1);
                    LLVMAddIncoming(phi, [zero].as_mut_ptr(), [empty_bb].as_mut_ptr(), 1);

                    Ok(Some(phi))
                }

                "channel_close" => {
                    let chan_handle = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    let chan_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        chan_handle,
                        i8_ptr_ty,
                        c"chan.ptr".as_ptr(),
                    );

                    // Destroy mutex and condvar
                    let pthread_mutex_destroy_fn =
                        *self.functions.get("pthread_mutex_destroy").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_mutex_destroy")
                        })?;
                    let pthread_mutex_destroy_ty = LLVMGlobalGetValueType(pthread_mutex_destroy_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_mutex_destroy_ty,
                        pthread_mutex_destroy_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    let condvar_offset = LLVMConstInt(i64_ty, 64, 0);
                    let condvar_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [condvar_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let pthread_cond_destroy_fn =
                        *self.functions.get("pthread_cond_destroy").ok_or_else(|| {
                            CompilerError::codegen_error("Missing pthread_cond_destroy")
                        })?;
                    let pthread_cond_destroy_ty = LLVMGlobalGetValueType(pthread_cond_destroy_fn);
                    LLVMBuildCall2(
                        self.builder,
                        pthread_cond_destroy_ty,
                        pthread_cond_destroy_fn,
                        [condvar_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    // Free buffer
                    let buf_ptr_offset = LLVMConstInt(i64_ty, 128, 0);
                    let buf_ptr_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        chan_ptr,
                        [buf_ptr_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let buf_ptr_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        buf_ptr_loc,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let buffer = LLVMBuildLoad2(
                        self.builder,
                        i8_ptr_ty,
                        buf_ptr_loc_typed,
                        c"buffer".as_ptr(),
                    );

                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [buffer].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    // Free channel struct
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [chan_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "pool_new" => {
                    // Create thread pool: allocate struct with channel and thread handles
                    // Pool layout:
                    //   0-7: work channel handle (int)
                    //   8-15: num_threads (int)
                    //   16-23: shutdown flag (int)
                    //   24+: thread handles array (num_threads * 8 bytes)
                    let num_threads = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);

                    // Allocate pool struct (base 24 bytes + thread handles)
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);

                    let base_size = LLVMConstInt(i64_ty, 24, 0);
                    let eight = LLVMConstInt(i64_ty, 8, 0);
                    let handles_size = LLVMBuildMul(self.builder, num_threads, eight, c"".as_ptr());
                    let pool_size =
                        LLVMBuildAdd(self.builder, base_size, handles_size, c"pool.size".as_ptr());

                    let pool_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [pool_size].as_mut_ptr(),
                        1,
                        c"pool".as_ptr(),
                    );

                    // Store num_threads at offset 8
                    let num_offset = LLVMConstInt(i64_ty, 8, 0);
                    let num_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        pool_ptr,
                        [num_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let num_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        num_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, num_threads, num_loc_typed);

                    // Initialize shutdown flag to 0
                    let shutdown_offset = LLVMConstInt(i64_ty, 16, 0);
                    let shutdown_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        pool_ptr,
                        [shutdown_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let shutdown_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        shutdown_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    LLVMBuildStore(self.builder, zero, shutdown_loc_typed);

                    // Note: Work channel and worker threads would be created here
                    // For now, store 0 as placeholder for channel handle
                    let chan_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        pool_ptr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, zero, chan_loc_typed);

                    let result =
                        LLVMBuildPtrToInt(self.builder, pool_ptr, i64_ty, c"pool.handle".as_ptr());
                    Ok(Some(result))
                }

                "pool_spawn" => {
                    // Submit work to thread pool (simplified: just evaluate args for now)
                    let _pool_handle = self.codegen_expression(&arguments[0])?;
                    let _func_ptr = self.codegen_expression(&arguments[1])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    // TODO: Send function pointer to work channel for workers to pick up
                    // For now, this is a no-op placeholder
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "pool_shutdown" => {
                    // Shutdown thread pool
                    let pool_handle = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

                    let pool_ptr = LLVMBuildIntToPtr(
                        self.builder,
                        pool_handle,
                        i8_ptr_ty,
                        c"pool.ptr".as_ptr(),
                    );

                    // Set shutdown flag to 1
                    let shutdown_offset = LLVMConstInt(i64_ty, 16, 0);
                    let shutdown_loc = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        pool_ptr,
                        [shutdown_offset].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let shutdown_loc_typed = LLVMBuildBitCast(
                        self.builder,
                        shutdown_loc,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    LLVMBuildStore(self.builder, one, shutdown_loc_typed);

                    // Free pool struct
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [pool_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );

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
