use std::collections::HashMap;
use crate::error::{CompilerError, CompilerResult, SourceLocation};
use crate::parser::ast::*;
use crate::lexer::token::Operator;
use super::types::{TypeEnvironment, FunctionType, StructType};
use std::path::PathBuf;

/// Type checker for Kraken AST.
/// 
/// Performs semantic analysis and type checking on the parsed AST.
pub struct TypeChecker {
    env: TypeEnvironment,
    file_path: PathBuf,
    current_function_return_type: Option<Type>,
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new(file_path: PathBuf) -> Self {
        let mut env = TypeEnvironment::new();
        
        // Add standard library functions
        env.define_function(
            "printf".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        env.define_function(
            "puts".to_string(),
            FunctionType {
                parameter_types: vec![Type::String],
                return_type: Type::Int,
                is_async: false,
            },
        );
        
        // String functions
        env.define_function("strlen".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("strcmp".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("strcpy".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("strcat".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("strstr".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("strchr".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::Int],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("strncpy".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String, Type::Int],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("strncmp".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String, Type::Int],
            return_type: Type::Int,
            is_async: false,
        });
        
        // Memory functions
        env.define_function("malloc".to_string(), FunctionType {
            parameter_types: vec![Type::Int],
            return_type: Type::String, // void* represented as string for now
            is_async: false,
        });
        env.define_function("free".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Void,
            is_async: false,
        });
        env.define_function("realloc".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::Int],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("memcpy".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String, Type::Int],
            return_type: Type::String,
            is_async: false,
        });
        
        // Math functions
        env.define_function("sqrt".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("pow".to_string(), FunctionType {
            parameter_types: vec![Type::Float, Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("abs".to_string(), FunctionType {
            parameter_types: vec![Type::Int],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("fabs".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("floor".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("ceil".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("round".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("sin".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("cos".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("tan".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("log".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("log10".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("exp".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        
        // Random functions
        env.define_function("rand".to_string(), FunctionType {
            parameter_types: vec![],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("srand".to_string(), FunctionType {
            parameter_types: vec![Type::Int],
            return_type: Type::Void,
            is_async: false,
        });
        
        // Time functions
        env.define_function("time".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        
        // File I/O functions (FILE* represented as String)
        env.define_function("fopen".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("fclose".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("fread".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::Int, Type::Int, Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("fwrite".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::Int, Type::Int, Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("fgets".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::Int, Type::String],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("fputs".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("fgetc".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("fputc".to_string(), FunctionType {
            parameter_types: vec![Type::Int, Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("fseek".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::Int, Type::Int],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("ftell".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("rewind".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Void,
            is_async: false,
        });
        env.define_function("fflush".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("feof".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("ferror".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("remove".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("rename".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        
        // System & Process functions
        env.define_function("exit".to_string(), FunctionType {
            parameter_types: vec![Type::Int],
            return_type: Type::Void,
            is_async: false,
        });
        env.define_function("system".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("getenv".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("setenv".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String, Type::Int],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("unsetenv".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        
        // String conversion functions
        env.define_function("atoi".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("atof".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::Float,
            is_async: false,
        });
        
        // Advanced math functions
        env.define_function("asin".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("acos".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("atan".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("atan2".to_string(), FunctionType {
            parameter_types: vec![Type::Float, Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("sinh".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("cosh".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("tanh".to_string(), FunctionType {
            parameter_types: vec![Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        env.define_function("fmod".to_string(), FunctionType {
            parameter_types: vec![Type::Float, Type::Float],
            return_type: Type::Float,
            is_async: false,
        });
        
        // Sleep function
        env.define_function("usleep".to_string(), FunctionType {
            parameter_types: vec![Type::Int],
            return_type: Type::Int,
            is_async: false,
        });
        
        // Character classification
        for func in ["isalpha", "isdigit", "isalnum", "isspace", "isupper", "islower", "toupper", "tolower"] {
            env.define_function(func.to_string(), FunctionType {
                parameter_types: vec![Type::Int],
                return_type: Type::Int,
                is_async: false,
            });
        }
        
        // String utilities
        env.define_function("strdup".to_string(), FunctionType {
            parameter_types: vec![Type::String],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("strtok".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::String,
            is_async: false,
        });
        
        // Memory operations
        env.define_function("memset".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::Int, Type::Int],
            return_type: Type::String,
            is_async: false,
        });
        env.define_function("memcmp".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String, Type::Int],
            return_type: Type::Int,
            is_async: false,
        });
        
        // Error handling
        env.define_function("abort".to_string(), FunctionType {
            parameter_types: vec![],
            return_type: Type::Void,
            is_async: false,
        });
        
        // Additional I/O
        env.define_function("putchar".to_string(), FunctionType {
            parameter_types: vec![Type::Int],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("getchar".to_string(), FunctionType {
            parameter_types: vec![],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("sprintf".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        env.define_function("sscanf".to_string(), FunctionType {
            parameter_types: vec![Type::String, Type::String],
            return_type: Type::Int,
            is_async: false,
        });
        
        Self {
            env,
            file_path,
            current_function_return_type: None,
        }
    }

    /// Type check a program.
    /// 
    /// # Arguments
    /// * `program` - The AST program to check
    /// 
    /// # Returns
    /// Ok if type checking succeeds
    /// 
    /// # Errors
    /// Returns `CompilerError::TypeError` if type checking fails
    pub fn check_program(&mut self, program: &Program) -> CompilerResult<()> {
        for statement in &program.statements {
            self.check_statement(statement)?;
        }
        Ok(())
    }

    /// Type check a statement.
    fn check_statement(&mut self, statement: &Statement) -> CompilerResult<()> {
        match statement {
            Statement::VariableDeclaration {
                name,
                type_annotation,
                initializer,
                is_mutable: _,
            } => {
                let var_type = if let Some(init) = initializer {
                    let init_type = self.check_expression(init)?;
                    
                    if let Some(annotation) = type_annotation {
                        if !self.types_compatible(annotation, &init_type) {
                            return Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!(
                                    "Type mismatch: expected {annotation}, found {init_type}"
                                ),
                            ));
                        }
                        annotation.clone()
                    } else {
                        init_type
                    }
                } else if let Some(annotation) = type_annotation {
                    annotation.clone()
                } else {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Variable declaration must have type annotation or initializer",
                    ));
                };

                self.env.define_variable(name.clone(), var_type);
                Ok(())
            }

            Statement::ConstantDeclaration {
                name,
                type_annotation,
                initializer,
            } => {
                let init_type = self.check_expression(initializer)?;
                
                if let Some(annotation) = type_annotation {
                    if !self.types_compatible(annotation, &init_type) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!(
                                "Type mismatch: expected {annotation}, found {init_type}"
                            ),
                        ));
                    }
                }

                self.env.define_variable(name.clone(), init_type);
                Ok(())
            }

            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
                is_async,
                is_public: _,
            } => {
                let param_types: Vec<Type> = parameters.iter().map(|p| p.param_type.clone()).collect();
                let ret_type = return_type.clone().unwrap_or(Type::Void);

                let func_type = FunctionType::new(param_types, ret_type.clone(), *is_async);
                self.env.define_function(name.clone(), func_type);

                let previous_return_type = self.current_function_return_type.clone();
                self.current_function_return_type = Some(ret_type);

                let mut func_env = self.env.child();
                for param in parameters {
                    func_env.define_variable(param.name.clone(), param.param_type.clone());
                }

                let saved_env = std::mem::replace(&mut self.env, func_env);
                self.check_block(body)?;
                self.env = saved_env;

                self.current_function_return_type = previous_return_type;
                Ok(())
            }

            Statement::StructDeclaration {
                name,
                fields,
                is_public: _,
            } => {
                let mut field_map = HashMap::new();
                for field in fields {
                    field_map.insert(field.name.clone(), field.field_type.clone());
                }

                let struct_type = StructType::new(field_map);
                self.env.define_struct(name.clone(), struct_type);
                Ok(())
            }

            Statement::ClassDeclaration {
                name,
                fields,
                methods,
                is_public: _,
            } => {
                let mut field_map = HashMap::new();
                for field in fields {
                    field_map.insert(field.name.clone(), field.field_type.clone());
                }

                let struct_type = StructType::new(field_map);
                self.env.define_struct(name.clone(), struct_type);

                for method in methods {
                    self.check_statement(method)?;
                }

                Ok(())
            }

            Statement::InterfaceDeclaration { name: _, methods: _ } => {
                // Interface checking would be implemented here
                Ok(())
            }

            Statement::Return { value } => {
                let return_type = if let Some(expr) = value {
                    self.check_expression(expr)?
                } else {
                    Type::Void
                };

                if let Some(expected) = &self.current_function_return_type {
                    if !self.types_compatible(expected, &return_type) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!(
                                "Return type mismatch: expected {expected}, found {return_type}"
                            ),
                        ));
                    }
                }

                Ok(())
            }

            Statement::Expression(expr) => {
                self.check_expression(expr)?;
                Ok(())
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.check_expression(condition)?;
                if cond_type != Type::Bool {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("If condition must be bool, found {cond_type}"),
                    ));
                }

                self.check_block(then_branch)?;
                
                if let Some(else_block) = else_branch {
                    self.check_block(else_block)?;
                }

                Ok(())
            }

            Statement::While { condition, body } => {
                let cond_type = self.check_expression(condition)?;
                if cond_type != Type::Bool {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("While condition must be bool, found {cond_type}"),
                    ));
                }

                self.check_block(body)?;
                Ok(())
            }

            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                if let Some(init) = initializer {
                    self.check_statement(init)?;
                }

                if let Some(cond) = condition {
                    let cond_type = self.check_expression(cond)?;
                    if cond_type != Type::Bool {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("For condition must be bool, found {cond_type}"),
                        ));
                    }
                }

                if let Some(inc) = increment {
                    self.check_expression(inc)?;
                }

                self.check_block(body)?;
                Ok(())
            }

            Statement::Match { expression, arms } => {
                let expr_type = self.check_expression(expression)?;

                for arm in arms {
                    // Check pattern compatibility with expression type
                    match &arm.pattern {
                        Pattern::Literal(lit_expr) => {
                            let lit_type = self.check_expression(lit_expr)?;
                            if !self.types_compatible(&expr_type, &lit_type) {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!(
                                        "Pattern type mismatch: expected {expr_type}, found {lit_type}"
                                    ),
                                ));
                            }
                        }
                        Pattern::Identifier(name) => {
                            let mut arm_env = self.env.child();
                            arm_env.define_variable(name.clone(), expr_type.clone());
                            let saved_env = std::mem::replace(&mut self.env, arm_env);
                            self.check_block(&arm.body)?;
                            self.env = saved_env;
                            continue;
                        }
                        Pattern::Wildcard => {}
                    }

                    self.check_block(&arm.body)?;
                }

                Ok(())
            }

            Statement::Break | Statement::Continue => Ok(()),

            Statement::Defer { statement } => {
                self.check_statement(statement)
            }
        }
    }

    /// Type check a block.
    fn check_block(&mut self, block: &Block) -> CompilerResult<()> {
        let block_env = self.env.child();
        let saved_env = std::mem::replace(&mut self.env, block_env);

        for statement in &block.statements {
            self.check_statement(statement)?;
        }

        self.env = saved_env;
        Ok(())
    }

    /// Type check an expression and return its type.
    fn check_expression(&mut self, expression: &Expression) -> CompilerResult<Type> {
        match expression {
            Expression::IntLiteral(_) => Ok(Type::Int),
            Expression::FloatLiteral(_) => Ok(Type::Float),
            Expression::StringLiteral(_) => Ok(Type::String),
            Expression::BoolLiteral(_) => Ok(Type::Bool),
            Expression::NullLiteral => Ok(Type::Void),

            Expression::Identifier(name) => {
                self.env.lookup_variable(name).ok_or_else(|| {
                    CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Undefined variable: {name}"),
                    )
                })
            }

            Expression::Binary { left, operator, right } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                self.check_binary_operation(&left_type, operator, &right_type)
            }

            Expression::Unary { operator, operand } => {
                let operand_type = self.check_expression(operand)?;
                self.check_unary_operation(operator, &operand_type)
            }

            Expression::Call { callee, arguments } => {
                // Check if it's a function call first (before checking as variable)
                if let Expression::Identifier(func_name) = callee.as_ref() {
                    if let Some(func_type) = self.env.lookup_function(func_name) {
                        if arguments.len() != func_type.parameter_types.len() {
                            return Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!(
                                    "Function {} expects {} arguments, found {}",
                                    func_name,
                                    func_type.parameter_types.len(),
                                    arguments.len()
                                ),
                            ));
                        }

                        for (i, arg) in arguments.iter().enumerate() {
                            let arg_type = self.check_expression(arg)?;
                            let expected_type = &func_type.parameter_types[i];
                            
                            if !self.types_compatible(expected_type, &arg_type) {
                                return Err(CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!(
                                        "Argument {} type mismatch: expected {}, found {}",
                                        i + 1,
                                        expected_type,
                                        arg_type
                                    ),
                                ));
                            }
                        }

                        return Ok(func_type.return_type.clone());
                    }
                }

                Err(CompilerError::type_error(
                    SourceLocation::new(self.file_path.clone(), 0, 0),
                    "Invalid function call",
                ))
            }

            Expression::Array { elements } => {
                if elements.is_empty() {
                    return Ok(Type::Array {
                        element_type: Box::new(Type::Void),
                        size: Some(0),
                    });
                }

                let first_type = self.check_expression(&elements[0])?;
                
                for elem in &elements[1..] {
                    let elem_type = self.check_expression(elem)?;
                    if !self.types_compatible(&first_type, &elem_type) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            "Array elements must have the same type",
                        ));
                    }
                }

                Ok(Type::Array {
                    element_type: Box::new(first_type),
                    size: Some(elements.len()),
                })
            }

            Expression::Index { array, index } => {
                let array_type = self.check_expression(array)?;
                let index_type = self.check_expression(index)?;

                if index_type != Type::Int {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Array index must be int, found {index_type}"),
                    ));
                }

                match array_type {
                    Type::Array { element_type, .. } => Ok(*element_type),
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Cannot index non-array type",
                    )),
                }
            }

            Expression::MemberAccess { object, member } => {
                let object_type = self.check_expression(object)?;

                match object_type {
                    Type::Custom(struct_name) => {
                        if let Some(struct_type) = self.env.lookup_struct(&struct_name) {
                            struct_type.get_field_type(member).cloned().ok_or_else(|| {
                                CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!("Struct {struct_name} has no field {member}"),
                                )
                            })
                        } else {
                            Err(CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!("Undefined struct: {struct_name}"),
                            ))
                        }
                    }
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Cannot access member of non-struct type",
                    )),
                }
            }

            Expression::StructLiteral { name, fields } => {
                // Look up the struct type
                let struct_type = self.env.lookup_struct(name).ok_or_else(|| {
                    CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Undefined struct: {name}"),
                    )
                })?;

                // Check that all fields are provided and have correct types
                for (field_name, field_expr) in fields {
                    let field_type = struct_type.get_field_type(field_name).ok_or_else(|| {
                        CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Struct {name} has no field {field_name}"),
                        )
                    })?;

                    let expr_type = self.check_expression(field_expr)?;
                    if !self.types_compatible(field_type, &expr_type) {
                        return Err(CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!(
                                "Field {field_name} expects type {field_type}, found {expr_type}"
                            ),
                        ));
                    }
                }

                Ok(Type::Custom(name.clone()))
            }

            Expression::Assignment { target, value } => {
                let target_type = self.check_expression(target)?;
                let value_type = self.check_expression(value)?;

                if !self.types_compatible(&target_type, &value_type) {
                    return Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!(
                            "Assignment type mismatch: expected {target_type}, found {value_type}"
                        ),
                    ));
                }

                Ok(value_type)
            }

            Expression::Reference { expression } => {
                let inner_type = self.check_expression(expression)?;
                Ok(Type::Reference {
                    inner_type: Box::new(inner_type),
                    is_mutable: false,
                })
            }

            Expression::Dereference { expression } => {
                let expr_type = self.check_expression(expression)?;
                match expr_type {
                    Type::Reference { inner_type, .. } => Ok(*inner_type),
                    _ => Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        "Cannot dereference non-reference type",
                    )),
                }
            }
        }
    }

    /// Check binary operation type compatibility.
    fn check_binary_operation(
        &self,
        left: &Type,
        operator: &Operator,
        right: &Type,
    ) -> CompilerResult<Type> {
        match operator {
            Operator::Plus | Operator::Minus | Operator::Star | Operator::Slash | Operator::Percent => {
                if (left == &Type::Int || left == &Type::Float)
                    && (right == &Type::Int || right == &Type::Float)
                {
                    if left == right {
                        Ok(left.clone())
                    } else {
                        Ok(Type::Float)
                    }
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Invalid operands for {operator}: {left} and {right}"),
                    ))
                }
            }

            Operator::Equal | Operator::NotEqual => {
                if self.types_compatible(left, right) {
                    Ok(Type::Bool)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot compare {left} and {right}"),
                    ))
                }
            }

            Operator::Less | Operator::LessEqual | Operator::Greater | Operator::GreaterEqual => {
                if (left == &Type::Int || left == &Type::Float)
                    && (right == &Type::Int || right == &Type::Float)
                {
                    Ok(Type::Bool)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot compare {left} and {right}"),
                    ))
                }
            }

            Operator::And | Operator::Or => {
                if left == &Type::Bool && right == &Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Logical operators require bool operands, found {left} and {right}"),
                    ))
                }
            }

            _ => Err(CompilerError::type_error(
                SourceLocation::new(self.file_path.clone(), 0, 0),
                format!("Unsupported binary operator: {operator}"),
            )),
        }
    }

    /// Check unary operation type compatibility.
    fn check_unary_operation(&self, operator: &Operator, operand: &Type) -> CompilerResult<Type> {
        match operator {
            Operator::Minus => {
                if operand == &Type::Int || operand == &Type::Float {
                    Ok(operand.clone())
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Cannot negate {operand}"),
                    ))
                }
            }

            Operator::Not => {
                if operand == &Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(CompilerError::type_error(
                        SourceLocation::new(self.file_path.clone(), 0, 0),
                        format!("Logical not requires bool operand, found {operand}"),
                    ))
                }
            }

            _ => Err(CompilerError::type_error(
                SourceLocation::new(self.file_path.clone(), 0, 0),
                format!("Unsupported unary operator: {operator}"),
            )),
        }
    }

    /// Check if two types are compatible.
    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        expected == actual
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenizer::Tokenizer;
    use crate::parser::parser::Parser;

    fn type_check_source(source: &str) -> CompilerResult<()> {
        let mut tokenizer = Tokenizer::new(source.to_string(), PathBuf::from("test.kr"));
        let tokens = tokenizer.tokenize()?;
        let mut parser = Parser::new(tokens, PathBuf::from("test.kr"));
        let program = parser.parse()?;
        let mut checker = TypeChecker::new(PathBuf::from("test.kr"));
        checker.check_program(&program)
    }

    #[test]
    fn test_variable_declaration() {
        assert!(type_check_source("let x: int = 42;").is_ok());
        assert!(type_check_source("let x = 42;").is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let result = type_check_source("let x: int = 3.14;");
        assert!(result.is_err());
    }

    #[test]
    fn test_function_declaration() {
        let source = r#"
            fn add(a: int, b: int) -> int {
                return a + b;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }

    #[test]
    fn test_if_statement() {
        let source = r#"
            let x = 5;
            if (x > 0) {
                let y = 10;
            }
        "#;
        assert!(type_check_source(source).is_ok());
    }
}
