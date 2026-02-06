//! Derive macro code generation for automatic trait implementations.

use crate::error::{CompilerError, CompilerResult};
use crate::parser::ast::{Block, Expression, Parameter, Statement, Type};

/// Derive macro code generator
pub struct DeriveGenerator;

impl DeriveGenerator {
    /// Create a new derive generator.
    pub fn new() -> Self {
        Self
    }

    /// Generate trait implementation for a struct
    pub fn generate_trait_impl(
        &self,
        trait_name: &str,
        type_name: &str,
        fields: &[(String, Type)],
    ) -> CompilerResult<Statement> {
        match trait_name {
            "Clone" => self.generate_clone_impl(type_name, fields),
            "Debug" => self.generate_debug_impl(type_name, fields),
            "PartialEq" => self.generate_partial_eq_impl(type_name, fields),
            "Eq" => Ok(self.generate_eq_impl(type_name)),
            "PartialOrd" => self.generate_partial_ord_impl(type_name, fields),
            "Ord" => self.generate_ord_impl(type_name, fields),
            "Hash" => self.generate_hash_impl(type_name, fields),
            _ => Err(CompilerError::internal_error(format!(
                "Unknown derive trait: {trait_name}"
            ))),
        }
    }

    fn generate_clone_impl(
        &self,
        type_name: &str,
        fields: &[(String, Type)],
    ) -> CompilerResult<Statement> {
        // Generate: fn clone(&self) -> TypeName { TypeName { field1: self.field1.clone(), ... } }
        let mut field_clones = Vec::new();
        for (field_name, _) in fields {
            field_clones.push((
                field_name.clone(),
                Expression::Call {
                    callee: Box::new(Expression::MemberAccess {
                        object: Box::new(Expression::MemberAccess {
                            object: Box::new(Expression::Identifier("self".to_string())),
                            member: field_name.clone(),
                        }),
                        member: "clone".to_string(),
                    }),
                    type_args: None,
                    arguments: vec![],
                },
            ));
        }

        let clone_body = Block {
            statements: vec![Statement::Return {
                value: Some(Expression::StructLiteral {
                    name: type_name.to_string(),
                    fields: field_clones,
                    type_args: None,
                }),
            }],
        };

        Ok(Statement::TraitImpl {
            trait_name: "Clone".to_string(),
            type_name: type_name.to_string(),
            generic_params: vec![],
            where_constraints: vec![],
            methods: vec![Statement::FunctionDeclaration {
                name: "clone".to_string(),
                generic_params: vec![],
                where_constraints: vec![],
                parameters: vec![Parameter {
                    pattern: crate::parser::ast::Pattern::Identifier("self".to_string()),
                    param_type: Type::Reference {
                        inner_type: Box::new(Type::Custom(type_name.to_string())),
                        is_mutable: false,
                    },
                    is_reference: true,
                }],
                return_type: Some(Type::Custom(type_name.to_string())),
                body: clone_body,
                is_async: false,
                is_unsafe: false,
                is_public: true,
                is_variadic: false,
            }],
        })
    }

    fn generate_debug_impl(
        &self,
        type_name: &str,
        _fields: &[(String, Type)],
    ) -> CompilerResult<Statement> {
        // Generate: fn debug(&self) -> str { ... }
        // Simplified implementation - just returns type name for now
        let debug_body = Block {
            statements: vec![Statement::Return {
                value: Some(Expression::StringLiteral(format!("{type_name} {{ ... }}"))),
            }],
        };

        Ok(Statement::TraitImpl {
            trait_name: "Debug".to_string(),
            type_name: type_name.to_string(),
            generic_params: vec![],
            where_constraints: vec![],
            methods: vec![Statement::FunctionDeclaration {
                name: "debug".to_string(),
                generic_params: vec![],
                where_constraints: vec![],
                parameters: vec![Parameter {
                    pattern: crate::parser::ast::Pattern::Identifier("self".to_string()),
                    param_type: Type::Reference {
                        inner_type: Box::new(Type::Custom(type_name.to_string())),
                        is_mutable: false,
                    },
                    is_reference: true,
                }],
                return_type: Some(Type::Str),
                body: debug_body,
                is_async: false,
                is_unsafe: false,
                is_public: true,
                is_variadic: false,
            }],
        })
    }

    fn generate_partial_eq_impl(
        &self,
        type_name: &str,
        fields: &[(String, Type)],
    ) -> CompilerResult<Statement> {
        // Generate: fn eq(&self, other: &TypeName) -> bool { self.field1 == other.field1 && ... }
        let mut comparisons = Vec::new();
        for (i, (field_name, _)) in fields.iter().enumerate() {
            let comparison = Expression::Binary {
                left: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Identifier("self".to_string())),
                    member: field_name.clone(),
                }),
                operator: crate::lexer::token::Operator::Equal,
                right: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Identifier("other".to_string())),
                    member: field_name.clone(),
                }),
            };

            if i == 0 {
                comparisons.push(comparison);
            } else {
                let prev = comparisons.pop().unwrap();
                comparisons.push(Expression::Binary {
                    left: Box::new(prev),
                    operator: crate::lexer::token::Operator::And,
                    right: Box::new(comparison),
                });
            }
        }

        let eq_expr = if comparisons.is_empty() {
            Expression::BoolLiteral(true)
        } else {
            comparisons.pop().unwrap()
        };

        let eq_body = Block {
            statements: vec![Statement::Return {
                value: Some(eq_expr),
            }],
        };

        Ok(Statement::TraitImpl {
            where_constraints: vec![],
            trait_name: "PartialEq".to_string(),
            type_name: type_name.to_string(),
            generic_params: vec![],
            methods: vec![Statement::FunctionDeclaration {
                name: "eq".to_string(),
                generic_params: vec![],
                where_constraints: vec![],
                parameters: vec![
                    Parameter {
                        pattern: crate::parser::ast::Pattern::Identifier("self".to_string()),
                        param_type: Type::Reference {
                            inner_type: Box::new(Type::Custom(type_name.to_string())),
                            is_mutable: false,
                        },
                        is_reference: true,
                    },
                    Parameter {
                        pattern: crate::parser::ast::Pattern::Identifier("other".to_string()),
                        param_type: Type::Reference {
                            inner_type: Box::new(Type::Custom(type_name.to_string())),
                            is_mutable: false,
                        },
                        is_reference: true,
                    },
                ],
                return_type: Some(Type::Bool),
                body: eq_body,
                is_async: false,
                is_unsafe: false,
                is_public: true,
                is_variadic: false,
            }],
        })
    }

    fn generate_eq_impl(&self, type_name: &str) -> Statement {
        // Eq is a marker trait, no methods needed
        Statement::TraitImpl {
            where_constraints: vec![],
            trait_name: "Eq".to_string(),
            type_name: type_name.to_string(),
            generic_params: vec![],
            methods: vec![],
        }
    }

    fn generate_partial_ord_impl(
        &self,
        type_name: &str,
        _fields: &[(String, Type)],
    ) -> CompilerResult<Statement> {
        // Simplified implementation
        Ok(Statement::TraitImpl {
            where_constraints: vec![],
            trait_name: "PartialOrd".to_string(),
            type_name: type_name.to_string(),
            generic_params: vec![],
            methods: vec![],
        })
    }

    fn generate_ord_impl(
        &self,
        type_name: &str,
        _fields: &[(String, Type)],
    ) -> CompilerResult<Statement> {
        // Simplified implementation
        Ok(Statement::TraitImpl {
            where_constraints: vec![],
            trait_name: "Ord".to_string(),
            type_name: type_name.to_string(),
            generic_params: vec![],
            methods: vec![],
        })
    }

    fn generate_hash_impl(
        &self,
        type_name: &str,
        _fields: &[(String, Type)],
    ) -> CompilerResult<Statement> {
        // Simplified implementation
        Ok(Statement::TraitImpl {
            where_constraints: vec![],
            trait_name: "Hash".to_string(),
            type_name: type_name.to_string(),
            generic_params: vec![],
            methods: vec![],
        })
    }
}

impl Default for DeriveGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_derive_generator_creation() {
        let generator = DeriveGenerator::new();
        assert!(std::mem::size_of_val(&generator) == 0);
    }

    #[test]
    fn test_generate_clone_impl() {
        let generator = DeriveGenerator::new();
        let fields = vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)];
        let result = generator.generate_clone_impl("Point", &fields);
        assert!(result.is_ok());
    }

    #[test]
    fn test_generate_debug_impl() {
        let generator = DeriveGenerator::new();
        let fields = vec![("value".to_string(), Type::Int)];
        let result = generator.generate_debug_impl("MyStruct", &fields);
        assert!(result.is_ok());
    }

    #[test]
    fn test_generate_partial_eq_impl() {
        let generator = DeriveGenerator::new();
        let fields = vec![("id".to_string(), Type::Int)];
        let result = generator.generate_partial_eq_impl("Entity", &fields);
        assert!(result.is_ok());
    }
}
