//! AST Desugaring Pass
//!
//! Transforms high-level syntactic constructs into simpler canonical forms
//! before type checking:
//! - `?` operator → match expression with early return
//!
//! This pass runs after parsing but before type checking.

use crate::error::CompilerResult;
use crate::parser::ast::*;

/// Desugar pass that transforms AST constructs into simpler forms.
pub struct AstDesugar {
    /// Counter for generating unique temporary variable names (reserved for ? desugaring)
    _temp_counter: u32,
}

impl Default for AstDesugar {
    fn default() -> Self {
        Self::new()
    }
}

impl AstDesugar {
    /// Create a new AST desugaring pass.
    pub fn new() -> Self {
        Self { _temp_counter: 0 }
    }

    /// Generate a unique temporary variable name (reserved for ? desugaring)
    fn _gen_temp(&mut self) -> String {
        let name = format!("__try_temp_{}", self._temp_counter);
        self._temp_counter += 1;
        name
    }

    /// Run desugaring on entire program
    pub fn desugar_program(&mut self, program: &mut Program) -> CompilerResult<()> {
        for statement in &mut program.statements {
            self.desugar_statement(statement)?;
        }
        Ok(())
    }

    /// Desugar a statement
    fn desugar_statement(&mut self, statement: &mut Statement) -> CompilerResult<()> {
        match statement {
            Statement::FunctionDeclaration { body, .. } => {
                self.desugar_block(body)?;
            }
            Statement::VariableDeclaration {
                initializer: Some(expr),
                ..
            } => {
                self.desugar_expression(expr)?;
            }
            Statement::VariableDeclaration {
                initializer: None, ..
            } => {}
            Statement::Expression(expression) => {
                self.desugar_expression(expression)?;
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.desugar_expression(condition)?;
                self.desugar_block(then_branch)?;
                if let Some(else_blk) = else_branch {
                    self.desugar_block(else_blk)?;
                }
            }
            Statement::While { condition, body } => {
                self.desugar_expression(condition)?;
                self.desugar_block(body)?;
            }
            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                if let Some(init) = initializer {
                    self.desugar_statement(init)?;
                }
                if let Some(cond) = condition {
                    self.desugar_expression(cond)?;
                }
                if let Some(inc) = increment {
                    self.desugar_expression(inc)?;
                }
                self.desugar_block(body)?;
            }
            Statement::Return { value: Some(expr) } => {
                self.desugar_expression(expr)?;
            }
            Statement::Return { value: None } => {}
            Statement::Match { expression, arms } => {
                self.desugar_expression(expression)?;
                for arm in arms {
                    if let Some(guard) = &mut arm.guard {
                        self.desugar_expression(guard)?;
                    }
                    self.desugar_block(&mut arm.body)?;
                }
            }
            Statement::Defer { statement } => {
                self.desugar_statement(statement)?;
            }
            Statement::Unsafe { block } => {
                self.desugar_block(block)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Desugar a block
    fn desugar_block(&mut self, block: &mut Block) -> CompilerResult<()> {
        for statement in &mut block.statements {
            self.desugar_statement(statement)?;
        }
        Ok(())
    }

    /// Desugar an expression
    #[allow(clippy::only_used_in_recursion)]
    fn desugar_expression(&mut self, expression: &mut Expression) -> CompilerResult<()> {
        match expression {
            Expression::Try { expression: inner } => {
                // Desugar ? operator to match expression
                // This is a placeholder - we'll implement the actual desugaring
                // after we understand the context better
                self.desugar_expression(inner)?;
            }
            Expression::Binary { left, right, .. } => {
                self.desugar_expression(left)?;
                self.desugar_expression(right)?;
            }
            Expression::Unary { operand, .. } => {
                self.desugar_expression(operand)?;
            }
            Expression::Call {
                callee, arguments, ..
            } => {
                self.desugar_expression(callee)?;
                for arg in arguments {
                    self.desugar_expression(arg)?;
                }
            }
            Expression::Array { elements } => {
                for elem in elements {
                    self.desugar_expression(elem)?;
                }
            }
            Expression::Index { array, index } => {
                self.desugar_expression(array)?;
                self.desugar_expression(index)?;
            }
            Expression::Slice { array, start, end } => {
                self.desugar_expression(array)?;
                self.desugar_expression(start)?;
                self.desugar_expression(end)?;
            }
            Expression::MemberAccess { object, .. } => {
                self.desugar_expression(object)?;
            }
            Expression::StructLiteral { fields, .. } => {
                for (_, field_expr) in fields {
                    self.desugar_expression(field_expr)?;
                }
            }
            Expression::Assignment { target, value } => {
                self.desugar_expression(target)?;
                self.desugar_expression(value)?;
            }
            Expression::Reference { expression: inner } => {
                self.desugar_expression(inner)?;
            }
            Expression::Dereference { expression: inner } => {
                self.desugar_expression(inner)?;
            }
            Expression::Await { expression: inner } => {
                self.desugar_expression(inner)?;
            }
            Expression::EnumVariant {
                payload: Some(payload_exprs),
                ..
            } => {
                for expr in payload_exprs {
                    self.desugar_expression(expr)?;
                }
            }
            Expression::EnumVariant { payload: None, .. } => {}
            Expression::Tuple { elements } => {
                for elem in elements {
                    self.desugar_expression(elem)?;
                }
            }
            Expression::TupleIndex { tuple, .. } => {
                self.desugar_expression(tuple)?;
            }
            Expression::Range { start, end, .. } => {
                self.desugar_expression(start)?;
                self.desugar_expression(end)?;
            }
            _ => {}
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::Operator;

    fn int_lit(v: i64) -> Expression {
        Expression::IntLiteral(v)
    }
    fn ident(s: &str) -> Expression {
        Expression::Identifier(s.to_string())
    }
    fn block(stmts: Vec<Statement>) -> Block {
        Block { statements: stmts }
    }
    fn empty_block() -> Block {
        Block { statements: vec![] }
    }
    fn expr_stmt(e: Expression) -> Statement {
        Statement::Expression(e)
    }
    fn bin(left: Expression, right: Expression) -> Expression {
        Expression::Binary {
            left: Box::new(left),
            operator: Operator::Plus,
            right: Box::new(right),
        }
    }

    #[test]
    fn test_desugar_empty_program() {
        let mut desugar = AstDesugar::new();
        let mut program = Program { statements: vec![] };
        assert!(desugar.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_default() {
        let d = AstDesugar::default();
        assert_eq!(d._temp_counter, 0);
    }

    #[test]
    fn test_gen_temp() {
        let mut d = AstDesugar::new();
        assert_eq!(d._gen_temp(), "__try_temp_0");
        assert_eq!(d._gen_temp(), "__try_temp_1");
        assert_eq!(d._gen_temp(), "__try_temp_2");
    }

    #[test]
    fn test_desugar_function_declaration() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::FunctionDeclaration {
                name: "foo".to_string(),
                generic_params: vec![],
                where_constraints: vec![],
                parameters: vec![],
                return_type: None,
                body: block(vec![expr_stmt(bin(int_lit(1), int_lit(2)))]),
                is_async: false,
                is_unsafe: false,
                is_public: false,
                is_variadic: false,
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_variable_declaration_with_init() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::VariableDeclaration {
                pattern: Pattern::Identifier("x".to_string()),
                type_annotation: None,
                initializer: Some(bin(int_lit(1), int_lit(2))),
                is_mutable: false,
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_variable_declaration_no_init() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::VariableDeclaration {
                pattern: Pattern::Identifier("x".to_string()),
                type_annotation: Some(Type::Int),
                initializer: None,
                is_mutable: false,
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_expression_statement() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(int_lit(42))],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_if_with_else() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::If {
                condition: Expression::BoolLiteral(true),
                then_branch: block(vec![expr_stmt(int_lit(1))]),
                else_branch: Some(block(vec![expr_stmt(int_lit(2))])),
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_if_without_else() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::If {
                condition: Expression::BoolLiteral(true),
                then_branch: empty_block(),
                else_branch: None,
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_while_loop() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::While {
                condition: Expression::BoolLiteral(true),
                body: block(vec![expr_stmt(int_lit(1))]),
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_for_loop_full() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::For {
                initializer: Some(Box::new(Statement::VariableDeclaration {
                    pattern: Pattern::Identifier("i".to_string()),
                    type_annotation: None,
                    initializer: Some(int_lit(0)),
                    is_mutable: true,
                })),
                condition: Some(bin(ident("i"), int_lit(10))),
                increment: Some(bin(ident("i"), int_lit(1))),
                body: empty_block(),
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_for_loop_empty() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::For {
                initializer: None,
                condition: None,
                increment: None,
                body: empty_block(),
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_return_with_value() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::Return {
                value: Some(bin(int_lit(1), int_lit(2))),
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_return_without_value() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::Return { value: None }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_match_with_guard() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::Match {
                expression: ident("x"),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(int_lit(1)),
                        guard: Some(Expression::BoolLiteral(true)),
                        body: block(vec![expr_stmt(int_lit(10))]),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: block(vec![expr_stmt(int_lit(0))]),
                    },
                ],
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_defer() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::Defer {
                statement: Box::new(expr_stmt(int_lit(1))),
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_unsafe_block() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::Unsafe {
                block: block(vec![expr_stmt(int_lit(42))]),
            }],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_break_continue() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![Statement::Break, Statement::Continue],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_try_expression() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Try {
                expression: Box::new(ident("result")),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_unary() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Unary {
                operator: Operator::Minus,
                operand: Box::new(int_lit(5)),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_call() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Call {
                callee: Box::new(ident("foo")),
                type_args: None,
                arguments: vec![int_lit(1), int_lit(2)],
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_array() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Array {
                elements: vec![int_lit(1), int_lit(2), int_lit(3)],
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_index() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Index {
                array: Box::new(ident("arr")),
                index: Box::new(int_lit(0)),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_slice() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Slice {
                array: Box::new(ident("arr")),
                start: Box::new(int_lit(0)),
                end: Box::new(int_lit(5)),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_member_access() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::MemberAccess {
                object: Box::new(ident("obj")),
                member: "field".to_string(),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_struct_literal() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::StructLiteral {
                name: "Point".to_string(),
                type_args: None,
                fields: vec![
                    ("x".to_string(), int_lit(1)),
                    ("y".to_string(), int_lit(2)),
                ],
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_assignment() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Assignment {
                target: Box::new(ident("x")),
                value: Box::new(int_lit(42)),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_reference() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Reference {
                expression: Box::new(ident("x")),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_dereference() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Dereference {
                expression: Box::new(ident("ptr")),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_await() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Await {
                expression: Box::new(ident("future")),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_enum_variant_with_payload() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::EnumVariant {
                enum_name: "Option".to_string(),
                variant_name: "Some".to_string(),
                payload: Some(vec![int_lit(42)]),
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_enum_variant_no_payload() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::EnumVariant {
                enum_name: "Option".to_string(),
                variant_name: "None".to_string(),
                payload: None,
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_tuple() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Tuple {
                elements: vec![int_lit(1), int_lit(2), int_lit(3)],
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_tuple_index() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::TupleIndex {
                tuple: Box::new(ident("t")),
                index: 0,
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_range() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Range {
                start: Box::new(int_lit(0)),
                end: Box::new(int_lit(10)),
                inclusive: false,
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_literals_passthrough() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![
                expr_stmt(Expression::FloatLiteral(3.14)),
                expr_stmt(Expression::StringLiteral("hello".to_string())),
                expr_stmt(Expression::BoolLiteral(false)),
                expr_stmt(Expression::NullLiteral),
            ],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_nested_expressions() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![expr_stmt(Expression::Call {
                callee: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Index {
                        array: Box::new(ident("arr")),
                        index: Box::new(bin(int_lit(1), int_lit(2))),
                    }),
                    member: "method".to_string(),
                }),
                type_args: None,
                arguments: vec![Expression::Unary {
                    operator: Operator::Minus,
                    operand: Box::new(Expression::Try {
                        expression: Box::new(ident("val")),
                    }),
                }],
            })],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }

    #[test]
    fn test_desugar_complex_program() {
        let mut d = AstDesugar::new();
        let mut program = Program {
            statements: vec![
                Statement::FunctionDeclaration {
                    name: "main".to_string(),
                    generic_params: vec![],
                    where_constraints: vec![],
                    parameters: vec![],
                    return_type: Some(Type::Int),
                    body: block(vec![
                        Statement::VariableDeclaration {
                            pattern: Pattern::Identifier("x".to_string()),
                            type_annotation: None,
                            initializer: Some(int_lit(10)),
                            is_mutable: true,
                        },
                        Statement::While {
                            condition: bin(ident("x"), int_lit(0)),
                            body: block(vec![
                                Statement::If {
                                    condition: bin(ident("x"), int_lit(5)),
                                    then_branch: block(vec![Statement::Break]),
                                    else_branch: None,
                                },
                                expr_stmt(Expression::Assignment {
                                    target: Box::new(ident("x")),
                                    value: Box::new(bin(ident("x"), int_lit(1))),
                                }),
                            ]),
                        },
                        Statement::Return {
                            value: Some(ident("x")),
                        },
                    ]),
                    is_async: false,
                    is_unsafe: false,
                    is_public: true,
                    is_variadic: false,
                },
            ],
        };
        assert!(d.desugar_program(&mut program).is_ok());
    }
}
