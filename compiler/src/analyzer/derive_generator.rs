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

    fn fields_xy() -> Vec<(String, Type)> {
        vec![("x".into(), Type::Int), ("y".into(), Type::Float)]
    }
    fn fields_single() -> Vec<(String, Type)> {
        vec![("id".into(), Type::Int)]
    }

    #[test]
    fn test_default() {
        let _g = DeriveGenerator;
    }

    #[test]
    fn test_new() {
        let g = DeriveGenerator::new();
        assert_eq!(std::mem::size_of_val(&g), 0);
    }

    // --- generate_trait_impl dispatch ---

    #[test]
    fn test_dispatch_clone() {
        let g = DeriveGenerator::new();
        let r = g.generate_trait_impl("Clone", "Pt", &fields_xy());
        assert!(r.is_ok());
        match r.unwrap() {
            Statement::TraitImpl {
                trait_name,
                type_name,
                methods,
                ..
            } => {
                assert_eq!(trait_name, "Clone");
                assert_eq!(type_name, "Pt");
                assert_eq!(methods.len(), 1);
            }
            _ => panic!("Expected TraitImpl"),
        }
    }

    #[test]
    fn test_dispatch_debug() {
        let g = DeriveGenerator::new();
        let r = g
            .generate_trait_impl("Debug", "S", &fields_single())
            .unwrap();
        match r {
            Statement::TraitImpl { trait_name, .. } => assert_eq!(trait_name, "Debug"),
            _ => panic!("Expected TraitImpl"),
        }
    }

    #[test]
    fn test_dispatch_partial_eq() {
        let g = DeriveGenerator::new();
        assert!(g
            .generate_trait_impl("PartialEq", "S", &fields_single())
            .is_ok());
    }

    #[test]
    fn test_dispatch_eq() {
        let g = DeriveGenerator::new();
        let r = g.generate_trait_impl("Eq", "S", &[]).unwrap();
        match r {
            Statement::TraitImpl {
                trait_name,
                methods,
                ..
            } => {
                assert_eq!(trait_name, "Eq");
                assert!(methods.is_empty()); // marker trait
            }
            _ => panic!("Expected TraitImpl"),
        }
    }

    #[test]
    fn test_dispatch_partial_ord() {
        let g = DeriveGenerator::new();
        assert!(g.generate_trait_impl("PartialOrd", "S", &[]).is_ok());
    }

    #[test]
    fn test_dispatch_ord() {
        let g = DeriveGenerator::new();
        assert!(g.generate_trait_impl("Ord", "S", &[]).is_ok());
    }

    #[test]
    fn test_dispatch_hash() {
        let g = DeriveGenerator::new();
        assert!(g.generate_trait_impl("Hash", "S", &[]).is_ok());
    }

    #[test]
    fn test_dispatch_unknown_trait() {
        let g = DeriveGenerator::new();
        assert!(g.generate_trait_impl("Serialize", "S", &[]).is_err());
    }

    // --- Clone impl details ---

    #[test]
    fn test_clone_empty_fields() {
        let g = DeriveGenerator::new();
        let r = g.generate_clone_impl("Empty", &[]).unwrap();
        match r {
            Statement::TraitImpl { methods, .. } => {
                assert_eq!(methods.len(), 1);
            }
            _ => panic!("Expected TraitImpl"),
        }
    }

    #[test]
    fn test_clone_multiple_fields() {
        let g = DeriveGenerator::new();
        let fields = vec![
            ("a".into(), Type::Int),
            ("b".into(), Type::String),
            ("c".into(), Type::Bool),
        ];
        assert!(g.generate_clone_impl("Multi", &fields).is_ok());
    }

    // --- PartialEq impl details ---

    #[test]
    fn test_partial_eq_empty_fields() {
        let g = DeriveGenerator::new();
        let r = g.generate_partial_eq_impl("Empty", &[]).unwrap();
        match r {
            Statement::TraitImpl { methods, .. } => {
                assert_eq!(methods.len(), 1);
                // Should return true for empty struct
                if let Statement::FunctionDeclaration { body, .. } = &methods[0] {
                    match &body.statements[0] {
                        Statement::Return {
                            value: Some(Expression::BoolLiteral(true)),
                        } => {}
                        _ => panic!("Expected return true for empty PartialEq"),
                    }
                }
            }
            _ => panic!("Expected TraitImpl"),
        }
    }

    #[test]
    fn test_partial_eq_single_field() {
        let g = DeriveGenerator::new();
        assert!(g.generate_partial_eq_impl("One", &fields_single()).is_ok());
    }

    #[test]
    fn test_partial_eq_multiple_fields() {
        let g = DeriveGenerator::new();
        let r = g.generate_partial_eq_impl("Multi", &fields_xy()).unwrap();
        // Should chain comparisons with &&
        match r {
            Statement::TraitImpl { methods, .. } => {
                assert_eq!(methods.len(), 1);
                if let Statement::FunctionDeclaration { body, .. } = &methods[0] {
                    match &body.statements[0] {
                        Statement::Return {
                            value: Some(Expression::Binary { operator, .. }),
                        } => {
                            assert_eq!(*operator, crate::lexer::token::Operator::And);
                        }
                        _ => panic!("Expected chained binary && for multi-field PartialEq"),
                    }
                }
            }
            _ => panic!("Expected TraitImpl"),
        }
    }

    // --- Debug impl ---

    #[test]
    fn test_debug_returns_type_name() {
        let g = DeriveGenerator::new();
        let r = g.generate_debug_impl("Widget", &[]).unwrap();
        match r {
            Statement::TraitImpl { methods, .. } => {
                if let Statement::FunctionDeclaration { body, .. } = &methods[0] {
                    match &body.statements[0] {
                        Statement::Return {
                            value: Some(Expression::StringLiteral(s)),
                        } => {
                            assert!(s.contains("Widget"));
                        }
                        _ => panic!("Expected string literal return"),
                    }
                }
            }
            _ => panic!("Expected TraitImpl"),
        }
    }
}
