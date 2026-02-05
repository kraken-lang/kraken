//! AST to IR Lowering
//!
//! Transforms the type-checked AST into Kraken IR.

use crate::analyzer::closure_analysis::ClosureAnalyzer;
use crate::error::{CompilerError, CompilerResult};
use crate::ir::types::{
    BlockId, IrBlock, IrFunction, IrInstruction, IrProgram, IrType, IrValue, ValueId,
};
use crate::parser::ast::{self, Block, Expression, Parameter, Pattern, Program, Statement, Type};

use super::types::*;

/// AST to IR lowering context.
pub struct IrLowering {
    /// Next available value ID.
    next_value_id: u32,
    /// Next available block ID.
    next_block_id: u32,
    /// Variable name to value ID mapping.
    variables: std::collections::HashMap<String, ValueId>,
    /// Variable name to struct type name mapping.
    var_struct_types: std::collections::HashMap<String, String>,
    /// Struct name to field names mapping.
    struct_fields: std::collections::HashMap<String, Vec<String>>,
    /// Function name to return type mapping.
    function_return_types: std::collections::HashMap<String, IrType>,
    /// Current function being lowered.
    current_function: Option<String>,
}

impl IrLowering {
    /// Extract a simple name from a pattern (for parameter names)
    fn extract_pattern_name(pattern: &Pattern) -> Option<String> {
        match pattern {
            Pattern::Identifier(name) => Some(name.clone()),
            Pattern::Tuple { patterns } => {
                // For tuple patterns, use first identifier or generate name
                patterns.iter().find_map(Self::extract_pattern_name)
            }
            Pattern::Struct { fields, .. } => {
                // For struct patterns, use first field name
                fields.first().map(|(name, _)| name.clone())
            }
            _ => None,
        }
    }

    pub fn new() -> Self {
        Self {
            next_value_id: 0,
            next_block_id: 0,
            variables: std::collections::HashMap::new(),
            var_struct_types: std::collections::HashMap::new(),
            struct_fields: std::collections::HashMap::new(),
            function_return_types: std::collections::HashMap::new(),
            current_function: None,
        }
    }

    /// Allocate a new value ID.
    fn alloc_value(&mut self) -> ValueId {
        let id = ValueId(self.next_value_id);
        self.next_value_id += 1;
        id
    }

    /// Allocate a new block ID.
    fn alloc_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block_id);
        self.next_block_id += 1;
        id
    }

    /// Lower an AST program to IR.
    pub fn lower_program(&mut self, program: &Program) -> CompilerResult<IrProgram> {
        let mut ir_program = IrProgram::new();

        for stmt in &program.statements {
            match stmt {
                Statement::StructDeclaration {
                    name,
                    generic_params: _,
                    where_constraints: _,
                    fields,
                    is_public,
                    repr: _,
                } => {
                    let ir_struct = self.lower_struct(name, fields, *is_public)?;
                    ir_program.structs.push(ir_struct);
                }
                Statement::FunctionDeclaration {
                    name,
                    generic_params: _,
                    where_constraints: _,
                    parameters,
                    return_type,
                    body,
                    is_public,
                    ..
                } => {
                    let ir_func = self.lower_function(
                        name,
                        parameters,
                        return_type.as_ref(),
                        body,
                        *is_public,
                    )?;
                    ir_program.functions.push(ir_func);
                }
                Statement::Module { .. } | Statement::Import { .. } => {
                    // Module/import handling is done at an earlier stage
                }
                Statement::ConstantDeclaration { .. } => {
                    // Constants are inlined at usage sites
                }
                _ => {
                    // Top-level statements other than functions/structs are not supported
                }
            }
        }

        Ok(ir_program)
    }

    /// Lower a struct declaration.
    fn lower_struct(
        &mut self,
        name: &str,
        fields: &[ast::StructField],
        is_public: bool,
    ) -> CompilerResult<IrStruct> {
        let ir_fields: Vec<(String, IrType)> = fields
            .iter()
            .map(|f| (f.name.clone(), Self::lower_type(&f.field_type)))
            .collect();

        // Register struct field names for MemberAccess lowering
        let field_names: Vec<String> = fields.iter().map(|f| f.name.clone()).collect();
        self.struct_fields.insert(name.to_string(), field_names);

        Ok(IrStruct {
            name: name.to_string(),
            fields: ir_fields,
            is_public,
        })
    }

    /// Lower a function declaration.
    fn lower_function(
        &mut self,
        name: &str,
        parameters: &[Parameter],
        return_type: Option<&Type>,
        body: &Block,
        is_public: bool,
    ) -> CompilerResult<IrFunction> {
        // Reset state for new function
        self.next_value_id = 0;
        self.next_block_id = 0;
        self.variables.clear();
        self.current_function = Some(name.to_string());

        // Convert parameters - extract names from patterns
        let ir_params: Vec<IrParam> = parameters
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let param_name =
                    Self::extract_pattern_name(&p.pattern).unwrap_or_else(|| format!("param_{i}"));
                IrParam {
                    name: param_name,
                    ty: Self::lower_type(&p.param_type),
                }
            })
            .collect();

        // Determine return type
        let ret_ty = return_type.map(Self::lower_type).unwrap_or(IrType::Void);

        // Register function return type for call lowering
        self.function_return_types
            .insert(name.to_string(), ret_ty.clone());

        let mut ir_func = IrFunction::new(name.to_string(), ir_params, ret_ty, is_public);

        // Create entry block
        let entry_block_id = self.alloc_block();
        let mut entry_block = IrBlock::new(entry_block_id, "entry".to_string());

        // Allocate parameters as local variables
        for (i, param) in parameters.iter().enumerate() {
            let param_name =
                Self::extract_pattern_name(&param.pattern).unwrap_or_else(|| format!("param_{i}"));
            let value_id = self.alloc_value();
            self.variables.insert(param_name.clone(), value_id);
            entry_block.instructions.push(IrInstruction::Alloca {
                dest: value_id,
                ty: Self::lower_type(&param.param_type),
                name: param_name.clone(),
            });
            // Store incoming parameter value
            let param_value = IrValue::Variable(param_name.clone());
            entry_block.instructions.push(IrInstruction::Store {
                value: param_value,
                ptr: IrValue::Register(value_id),
            });
        }

        // Lower function body
        self.lower_block(body, &mut entry_block)?;

        // Ensure block has a terminator
        if entry_block.instructions.is_empty()
            || !self.is_terminator(entry_block.instructions.last())
        {
            entry_block
                .instructions
                .push(IrInstruction::Return { value: None });
        }

        ir_func.blocks.push(entry_block);
        self.current_function = None;

        Ok(ir_func)
    }

    /// Lower a block of statements.
    fn lower_block(&mut self, block: &Block, ir_block: &mut IrBlock) -> CompilerResult<()> {
        for stmt in &block.statements {
            self.lower_statement(stmt, ir_block)?;
        }
        Ok(())
    }

    /// Lower a single statement.
    fn lower_statement(&mut self, stmt: &Statement, ir_block: &mut IrBlock) -> CompilerResult<()> {
        match stmt {
            Statement::VariableDeclaration {
                pattern,
                type_annotation,
                initializer,
                is_mutable: _,
            } => {
                let ty = type_annotation
                    .as_ref()
                    .map(Self::lower_type)
                    .unwrap_or(IrType::Int);

                // For simple identifier patterns, use existing logic
                if let Pattern::Identifier(name) = pattern {
                    // Track struct type for MemberAccess lowering
                    if let Some(Type::Custom(struct_name)) = type_annotation {
                        self.var_struct_types
                            .insert(name.clone(), struct_name.clone());
                    }

                    let value_id = self.alloc_value();
                    self.variables.insert(name.clone(), value_id);

                    ir_block.instructions.push(IrInstruction::Alloca {
                        dest: value_id,
                        ty: ty.clone(),
                        name: name.clone(),
                    });

                    if let Some(init) = initializer {
                        let init_value = self.lower_expression(init, ir_block)?;
                        ir_block.instructions.push(IrInstruction::Store {
                            value: init_value,
                            ptr: IrValue::Register(value_id),
                        });
                    }
                } else if let Pattern::Tuple { patterns } = pattern {
                    // Tuple destructuring: let (x, y) = tuple_expr;
                    if let Some(init) = initializer {
                        let _tuple_value = self.lower_expression(init, ir_block)?;

                        // For now, treat tuple destructuring as individual variable assignments
                        // The LLVM backend will handle the actual tuple element extraction
                        // We just need to track that these variables exist
                        for pat in patterns.iter() {
                            if let Pattern::Identifier(name) = pat {
                                let var_id = self.alloc_value();
                                self.variables.insert(name.clone(), var_id);

                                // Allocate variable (extraction happens at LLVM level)
                                ir_block.instructions.push(IrInstruction::Alloca {
                                    dest: var_id,
                                    ty: IrType::Int, // Type will be determined at LLVM level
                                    name: name.clone(),
                                });
                            }
                        }

                        // Store the tuple value for later extraction
                        // This is a simplified approach - full implementation would extract elements here
                    }
                }
            }

            Statement::Return { value } => {
                let ret_value = if let Some(expr) = value {
                    Some(self.lower_expression(expr, ir_block)?)
                } else {
                    None
                };
                ir_block
                    .instructions
                    .push(IrInstruction::Return { value: ret_value });
            }

            Statement::Expression(expr) => {
                self.lower_expression(expr, ir_block)?;
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_value = self.lower_expression(condition, ir_block)?;

                // For now, inline the branches (simplified - no actual CFG splitting)
                // Full CFG support would require multiple blocks
                let _ = cond_value;
                self.lower_block(then_branch, ir_block)?;
                if let Some(else_block) = else_branch {
                    self.lower_block(else_block, ir_block)?;
                }
            }

            Statement::While { condition, body } => {
                let _ = self.lower_expression(condition, ir_block)?;
                self.lower_block(body, ir_block)?;
            }

            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                if let Some(init) = initializer {
                    self.lower_statement(init, ir_block)?;
                }
                if let Some(cond) = condition {
                    self.lower_expression(cond, ir_block)?;
                }
                self.lower_block(body, ir_block)?;
                if let Some(inc) = increment {
                    self.lower_expression(inc, ir_block)?;
                }
            }

            Statement::ForIn {
                variable,
                iterable,
                body,
            } => {
                // Desugar for-in loop to while loop
                // for (i in start..end) { body }
                // becomes:
                // let i = start;
                // while (i < end) { body; i = i + 1; }

                // Extract range start and end from iterable
                if let Expression::Range {
                    start,
                    end,
                    inclusive,
                } = iterable
                {
                    // Allocate variable for loop counter
                    let value_id = self.alloc_value();
                    self.variables.insert(variable.clone(), value_id);
                    ir_block.instructions.push(IrInstruction::Alloca {
                        dest: value_id,
                        ty: IrType::Int,
                        name: variable.clone(),
                    });

                    // Initialize counter to start value
                    let start_val = self.lower_expression(start, ir_block)?;
                    ir_block.instructions.push(IrInstruction::Store {
                        value: start_val,
                        ptr: IrValue::Register(value_id),
                    });

                    // Lower the loop body (condition and increment handled by LLVM codegen)
                    // Store end value for comparison
                    let _end_val = self.lower_expression(end, ir_block)?;
                    let _inclusive = *inclusive;

                    // Lower body
                    self.lower_block(body, ir_block)?;
                } else {
                    return Err(CompilerError::codegen_error(
                        "For-in loop requires range expression",
                    ));
                }
            }

            Statement::Break | Statement::Continue => {
                // These require CFG support - simplified for v1
            }

            Statement::Defer { statement } => {
                // Defer is collected and inserted at function exit - deferred to 0.8.7
                let _ = statement;
            }

            Statement::Match { expression, arms } => {
                let _ = self.lower_expression(expression, ir_block)?;
                for arm in arms {
                    self.lower_block(&arm.body, ir_block)?;
                }
            }

            _ => {
                // Other statements handled elsewhere or not applicable in function body
            }
        }
        Ok(())
    }

    /// Lower an expression and return its IR value.
    fn lower_expression(
        &mut self,
        expr: &Expression,
        ir_block: &mut IrBlock,
    ) -> CompilerResult<IrValue> {
        match expr {
            Expression::IntLiteral(v) => Ok(IrValue::ConstInt(*v)),

            Expression::FloatLiteral(v) => Ok(IrValue::ConstFloat(*v)),

            Expression::StringLiteral(s) => Ok(IrValue::ConstString(s.clone())),

            Expression::BoolLiteral(v) => Ok(IrValue::ConstBool(*v)),

            Expression::NullLiteral => Ok(IrValue::Null),

            Expression::Identifier(name) => {
                if let Some(&value_id) = self.variables.get(name) {
                    // Load the variable
                    let dest = self.alloc_value();
                    ir_block.instructions.push(IrInstruction::Load {
                        dest,
                        ptr: IrValue::Register(value_id),
                        ty: IrType::Int, // Type inference simplified
                    });
                    Ok(IrValue::Register(dest))
                } else {
                    // Assume it's a function or external reference
                    Ok(IrValue::Variable(name.clone()))
                }
            }

            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.lower_expression(left, ir_block)?;
                let right_val = self.lower_expression(right, ir_block)?;
                let dest = self.alloc_value();

                ir_block.instructions.push(IrInstruction::BinaryOp {
                    dest,
                    op: *operator,
                    left: left_val,
                    right: right_val,
                    ty: IrType::Int, // Type inference simplified
                });

                Ok(IrValue::Register(dest))
            }

            Expression::Unary { operator, operand } => {
                let operand_val = self.lower_expression(operand, ir_block)?;
                let dest = self.alloc_value();

                ir_block.instructions.push(IrInstruction::UnaryOp {
                    dest,
                    op: *operator,
                    operand: operand_val,
                    ty: IrType::Int,
                });

                Ok(IrValue::Register(dest))
            }

            Expression::Call {
                callee,
                type_args: _,
                arguments,
            } => {
                let args: Vec<IrValue> = arguments
                    .iter()
                    .map(|a| self.lower_expression(a, ir_block))
                    .collect::<CompilerResult<Vec<_>>>()?;

                let func_name = match callee.as_ref() {
                    Expression::Identifier(name) => name.clone(),
                    _ => return Err(CompilerError::codegen_error("Complex callee not supported")),
                };

                let dest = self.alloc_value();
                // Look up the function's return type, default to Int for stdlib functions
                let ret_ty = self
                    .function_return_types
                    .get(&func_name)
                    .cloned()
                    .unwrap_or(IrType::Int);
                ir_block.instructions.push(IrInstruction::Call {
                    dest: Some(dest),
                    func: func_name,
                    args,
                    ret_ty,
                });

                Ok(IrValue::Register(dest))
            }

            Expression::Assignment { target, value } => {
                let value_ir = self.lower_expression(value, ir_block)?;

                match target.as_ref() {
                    Expression::Identifier(name) => {
                        if let Some(&ptr_id) = self.variables.get(name) {
                            ir_block.instructions.push(IrInstruction::Store {
                                value: value_ir.clone(),
                                ptr: IrValue::Register(ptr_id),
                            });
                        }
                    }
                    _ => {
                        // Complex assignment targets (array index, member access)
                        // Simplified for v1
                    }
                }

                Ok(value_ir)
            }

            Expression::Index { array, index } => {
                let array_val = self.lower_expression(array, ir_block)?;
                let index_val = self.lower_expression(index, ir_block)?;
                let dest = self.alloc_value();

                ir_block.instructions.push(IrInstruction::GetElementPtr {
                    dest,
                    ptr: array_val,
                    indices: vec![index_val],
                    ty: IrType::Int,
                });

                // Load the value
                let load_dest = self.alloc_value();
                ir_block.instructions.push(IrInstruction::Load {
                    dest: load_dest,
                    ptr: IrValue::Register(dest),
                    ty: IrType::Int,
                });

                Ok(IrValue::Register(load_dest))
            }

            Expression::Slice { array, start, end } => {
                // For IR lowering, we'll emit a call to str_slice intrinsic
                let _array_val = self.lower_expression(array, ir_block)?;
                let _start_val = self.lower_expression(start, ir_block)?;
                let _end_val = self.lower_expression(end, ir_block)?;
                // Slice operations handled directly by LLVM backend
                // IR representation deferred for optimization purposes
                Ok(IrValue::ConstInt(0))
            }

            Expression::MemberAccess { object, member } => {
                let obj_val = self.lower_expression(object, ir_block)?;
                let dest = self.alloc_value();

                // Try to find the struct type and field index
                let field_idx = if let Expression::Identifier(var_name) = object.as_ref() {
                    if let Some(struct_name) = self.var_struct_types.get(var_name) {
                        if let Some(fields) = self.struct_fields.get(struct_name) {
                            fields.iter().position(|f| f == member).unwrap_or(0) as u32
                        } else {
                            0
                        }
                    } else {
                        0
                    }
                } else {
                    0
                };

                ir_block.instructions.push(IrInstruction::ExtractValue {
                    dest,
                    ptr: obj_val,
                    field_idx,
                    ty: IrType::Int,
                });

                Ok(IrValue::Register(dest))
            }

            Expression::Array { elements } => {
                // Lower all elements
                for elem in elements {
                    self.lower_expression(elem, ir_block)?;
                }
                // Return placeholder - proper array handling needs more work
                Ok(IrValue::Null)
            }

            Expression::StructLiteral {
                name,
                type_args: _,
                fields,
            } => {
                for (_, expr) in fields {
                    self.lower_expression(expr, ir_block)?;
                }
                Ok(IrValue::Variable(name.clone()))
            }

            Expression::Reference { expression } => {
                // Get address of expression
                self.lower_expression(expression, ir_block)
            }

            Expression::Dereference { expression } => {
                let ptr_val = self.lower_expression(expression, ir_block)?;
                let dest = self.alloc_value();
                ir_block.instructions.push(IrInstruction::Load {
                    dest,
                    ptr: ptr_val,
                    ty: IrType::Int,
                });
                Ok(IrValue::Register(dest))
            }

            Expression::Await { expression } => {
                // Lower the awaited expression
                // This will be transformed by state machine lowering later
                let future_val = self.lower_expression(expression, ir_block)?;
                let dest = self.alloc_value();
                // Emit a special await call that state machine lowering will process
                ir_block.instructions.push(IrInstruction::Call {
                    dest: Some(dest),
                    func: "__await".to_string(),
                    args: vec![future_val],
                    ret_ty: IrType::Int, // Placeholder, actual type from future
                });
                Ok(IrValue::Register(dest))
            }

            Expression::Spawn { body } => {
                // Lower the spawn body into a separate function-like structure
                // For now, emit a spawn call with the body as a closure
                let dest = self.alloc_value();

                // Lower body statements
                for stmt in &body.statements {
                    self.lower_statement(stmt, ir_block)?;
                }

                // Emit spawn call (returns handle)
                ir_block.instructions.push(IrInstruction::Call {
                    dest: Some(dest),
                    func: "__spawn".to_string(),
                    args: vec![],
                    ret_ty: IrType::Bytes, // Handle type
                });
                Ok(IrValue::Register(dest))
            }

            Expression::EnumVariant { .. } => {
                // Enum variants are represented as integer tags
                // For IR purposes, just return a constant 0 (tag value computed at codegen)
                Ok(IrValue::ConstInt(0))
            }

            Expression::Tuple { elements } => {
                // Lower tuple elements
                let values: Vec<IrValue> = elements
                    .iter()
                    .map(|e| self.lower_expression(e, ir_block))
                    .collect::<CompilerResult<Vec<_>>>()?;

                // For now, create a tuple value (will be handled in codegen)
                let dest = self.alloc_value();
                ir_block.instructions.push(IrInstruction::Call {
                    dest: Some(dest),
                    func: "__tuple_create".to_string(),
                    args: values,
                    ret_ty: IrType::Tuple(vec![IrType::Int; elements.len()]),
                });
                Ok(IrValue::Register(dest))
            }

            Expression::TupleIndex { tuple, index } => {
                let tuple_val = self.lower_expression(tuple, ir_block)?;
                let dest = self.alloc_value();

                // Extract tuple element at index
                ir_block.instructions.push(IrInstruction::Call {
                    dest: Some(dest),
                    func: format!("__tuple_get_{index}"),
                    args: vec![tuple_val],
                    ret_ty: IrType::Int,
                });
                Ok(IrValue::Register(dest))
            }

            Expression::Try { expression } => {
                // Try operator should be desugared before IR lowering
                // For now, just lower the inner expression
                // TODO: Implement proper desugaring before this stage
                self.lower_expression(expression, ir_block)
            }

            Expression::Range {
                start,
                end,
                inclusive: _,
            } => {
                // Range expressions are only used in for-in loops
                // They don't produce a value themselves, just validate the bounds
                let _start_val = self.lower_expression(start, ir_block)?;
                let _end_val = self.lower_expression(end, ir_block)?;
                // Return a placeholder value
                Ok(IrValue::ConstInt(0))
            }

            Expression::Closure {
                parameters,
                return_type,
                body,
                is_move,
            } => {
                // Perform capture analysis
                let mut analyzer = ClosureAnalyzer::new();
                let _env = analyzer.analyze_closure(parameters, body, *is_move)?;

                // Generate unique closure environment struct name
                let closure_id = self.next_value_id;
                self.next_value_id += 1;
                let env_struct_name = format!("__closure_env_{closure_id}");

                // For now, create a placeholder value representing the closure
                // Full implementation would:
                // 1. Generate environment struct type with captured variable fields
                // 2. Allocate environment struct on heap
                // 3. Initialize captured variables in the struct
                // 4. Create function pointer with environment as context
                // 5. Return closure value (function pointer + environment pointer)

                // Placeholder: return a constant representing the closure
                // This allows the code to compile but closures can't be called yet
                // Full implementation requires:
                // - Environment struct allocation
                // - Captured variable initialization
                // - Function pointer creation

                // For now, return a placeholder value
                // The closure environment info is tracked in `env` for future use
                let _ = env_struct_name; // Suppress unused warning
                let _ = return_type; // Suppress unused warning

                Ok(IrValue::ConstInt(0))
            }
        }
    }

    /// Convert AST type to IR type.
    fn lower_type(ty: &Type) -> IrType {
        match ty {
            Type::Int => IrType::Int,
            Type::Float => IrType::Float,
            Type::Bool => IrType::Bool,
            Type::String => IrType::String,
            Type::Str => IrType::Str,
            Type::Bytes => IrType::Bytes,
            Type::Void => IrType::Void,
            Type::VecInt => IrType::VecInt,
            Type::VecString => IrType::VecString,
            Type::VecBytes => IrType::VecBytes,
            Type::MapStringInt => IrType::MapStringInt,
            Type::MapStringString => IrType::MapStringString,
            Type::SliceInt => IrType::SliceInt,
            Type::SliceString => IrType::SliceString,
            Type::SliceBytes => IrType::SliceBytes,
            Type::Array { element_type, size } => IrType::Array {
                element: Box::new(Self::lower_type(element_type)),
                size: *size,
            },
            Type::Reference { inner_type, .. }
            | Type::Pointer { inner_type, .. }
            | Type::RawPointer { inner_type, .. } => {
                IrType::Pointer(Box::new(Self::lower_type(inner_type)))
            }
            Type::Custom(name) => IrType::Struct(name.clone()),
            Type::Generic { name, .. } => IrType::Struct(name.clone()),
            Type::Tuple { element_types } => {
                // Lower tuple to struct with numbered fields
                IrType::Tuple(element_types.iter().map(Self::lower_type).collect())
            }
            Type::Function { .. } => {
                // Function types represented as void pointers in IR
                // Proper function pointer typing handled by LLVM backend
                IrType::Pointer(Box::new(IrType::Void))
            }
            Type::TraitObject { .. } => {
                // Trait objects represented as fat pointers (data ptr + vtable ptr)
                // Full fat pointer implementation deferred to trait system completion
                IrType::Pointer(Box::new(IrType::Void))
            }
        }
    }

    /// Check if an instruction is a terminator.
    fn is_terminator(&self, instr: Option<&IrInstruction>) -> bool {
        matches!(
            instr,
            Some(IrInstruction::Return { .. })
                | Some(IrInstruction::Branch { .. })
                | Some(IrInstruction::CondBranch { .. })
        )
    }
}

impl Default for IrLowering {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{Block, Program, Statement};

    #[test]
    fn test_lower_empty_program() {
        let mut lowering = IrLowering::new();
        let program = Program::new(vec![]);
        let ir = lowering.lower_program(&program).unwrap();
        assert!(ir.functions.is_empty());
        assert!(ir.structs.is_empty());
    }

    #[test]
    fn test_lower_simple_function() {
        let mut lowering = IrLowering::new();
        let program = Program::new(vec![Statement::FunctionDeclaration {
            name: "main".to_string(),
            generic_params: vec![],
            where_constraints: vec![],
            parameters: vec![],
            return_type: Some(Type::Int),
            body: Block::new(vec![]),
            is_async: false,
            is_unsafe: false,
            is_public: false,
            is_variadic: false,
        }]);
        let ir_program = lowering.lower_program(&program).unwrap();
        assert_eq!(ir_program.functions.len(), 1);
        assert_eq!(ir_program.functions[0].name, "main");
    }
}
