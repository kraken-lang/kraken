//! Compile-time constant evaluation for const functions and static assertions.

use crate::error::{CompilerError, CompilerResult};
use crate::parser::ast::{Block, Expression, Statement};
use std::collections::HashMap;

/// Constant value that can be evaluated at compile time
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

/// Constant evaluator for compile-time evaluation
pub struct ConstEvaluator {
    const_functions: HashMap<String, (Vec<String>, Block)>,
    const_values: HashMap<String, ConstValue>,
}

impl Default for ConstEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstEvaluator {
    /// Create a new constant evaluator with empty function and value registries.
    pub fn new() -> Self {
        Self {
            const_functions: HashMap::new(),
            const_values: HashMap::new(),
        }
    }

    /// Register a const function for evaluation
    pub fn register_const_function(&mut self, name: String, params: Vec<String>, body: Block) {
        self.const_functions.insert(name, (params, body));
    }

    /// Evaluate an expression at compile time
    pub fn eval_expression(&mut self, expr: &Expression) -> CompilerResult<ConstValue> {
        match expr {
            Expression::IntLiteral(n) => Ok(ConstValue::Int(*n)),
            Expression::FloatLiteral(f) => Ok(ConstValue::Float(*f)),
            Expression::BoolLiteral(b) => Ok(ConstValue::Bool(*b)),
            Expression::StringLiteral(s) => Ok(ConstValue::String(s.clone())),

            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.eval_expression(left)?;
                let right_val = self.eval_expression(right)?;
                self.eval_binary_op(&left_val, operator, &right_val)
            }

            Expression::Unary { operator, operand } => {
                let val = self.eval_expression(operand)?;
                self.eval_unary_op(operator, &val)
            }

            Expression::Identifier(name) => {
                if let Some(val) = self.const_values.get(name) {
                    Ok(val.clone())
                } else {
                    Err(CompilerError::internal_error(format!(
                        "Variable '{name}' is not a compile-time constant"
                    )))
                }
            }

            Expression::Call {
                callee,
                type_args: _,
                arguments,
            } => {
                if let Expression::Identifier(func_name) = &**callee {
                    self.eval_const_function_call(func_name, arguments)
                } else {
                    Err(CompilerError::internal_error(
                        "Only direct function calls can be evaluated at compile time".to_string(),
                    ))
                }
            }

            _ => Err(CompilerError::internal_error(
                "Expression cannot be evaluated at compile time".to_string(),
            )),
        }
    }

    fn eval_binary_op(
        &self,
        left: &ConstValue,
        op: &crate::lexer::token::Operator,
        right: &ConstValue,
    ) -> CompilerResult<ConstValue> {
        use crate::lexer::token::Operator;

        match (left, right) {
            (ConstValue::Int(l), ConstValue::Int(r)) => match op {
                Operator::Plus => Ok(ConstValue::Int(l + r)),
                Operator::Minus => Ok(ConstValue::Int(l - r)),
                Operator::Star => Ok(ConstValue::Int(l * r)),
                Operator::Slash => {
                    if *r == 0 {
                        Err(CompilerError::internal_error(
                            "Division by zero in const evaluation".to_string(),
                        ))
                    } else {
                        Ok(ConstValue::Int(l / r))
                    }
                }
                Operator::Percent => Ok(ConstValue::Int(l % r)),
                Operator::Equal => Ok(ConstValue::Bool(l == r)),
                Operator::NotEqual => Ok(ConstValue::Bool(l != r)),
                Operator::Less => Ok(ConstValue::Bool(l < r)),
                Operator::LessEqual => Ok(ConstValue::Bool(l <= r)),
                Operator::Greater => Ok(ConstValue::Bool(l > r)),
                Operator::GreaterEqual => Ok(ConstValue::Bool(l >= r)),
                Operator::BitAnd => Ok(ConstValue::Int(l & r)),
                Operator::BitOr => Ok(ConstValue::Int(l | r)),
                Operator::BitXor => Ok(ConstValue::Int(l ^ r)),
                Operator::LeftShift => Ok(ConstValue::Int(l << r)),
                Operator::RightShift => Ok(ConstValue::Int(l >> r)),
                _ => Err(CompilerError::internal_error(format!(
                    "Operator {op:?} not supported in const evaluation"
                ))),
            },
            (ConstValue::Bool(l), ConstValue::Bool(r)) => match op {
                Operator::And => Ok(ConstValue::Bool(*l && *r)),
                Operator::Or => Ok(ConstValue::Bool(*l || *r)),
                Operator::Equal => Ok(ConstValue::Bool(l == r)),
                Operator::NotEqual => Ok(ConstValue::Bool(l != r)),
                _ => Err(CompilerError::internal_error(format!(
                    "Operator {op:?} not supported for booleans"
                ))),
            },
            _ => Err(CompilerError::internal_error(
                "Type mismatch in const binary operation".to_string(),
            )),
        }
    }

    fn eval_unary_op(
        &self,
        op: &crate::lexer::token::Operator,
        val: &ConstValue,
    ) -> CompilerResult<ConstValue> {
        use crate::lexer::token::Operator;

        match (op, val) {
            (Operator::Minus, ConstValue::Int(n)) => Ok(ConstValue::Int(-n)),
            (Operator::Not, ConstValue::Bool(b)) => Ok(ConstValue::Bool(!b)),
            (Operator::BitNot, ConstValue::Int(n)) => Ok(ConstValue::Int(!n)),
            _ => Err(CompilerError::internal_error(format!(
                "Operator {op:?} not supported in const unary operation"
            ))),
        }
    }

    fn eval_const_function_call(
        &mut self,
        func_name: &str,
        arguments: &[Expression],
    ) -> CompilerResult<ConstValue> {
        if let Some((params, body)) = self.const_functions.get(func_name).cloned() {
            if params.len() != arguments.len() {
                return Err(CompilerError::internal_error(format!(
                    "Function '{}' expects {} arguments, got {}",
                    func_name,
                    params.len(),
                    arguments.len()
                )));
            }

            // Evaluate arguments
            let mut arg_values = Vec::new();
            for arg in arguments {
                arg_values.push(self.eval_expression(arg)?);
            }

            // Bind parameters to argument values
            let saved_values = self.const_values.clone();
            for (param, value) in params.iter().zip(arg_values.iter()) {
                self.const_values.insert(param.clone(), value.clone());
            }

            // Evaluate function body
            let result = self.eval_block(&body);

            // Restore previous const values
            self.const_values = saved_values;

            result
        } else {
            Err(CompilerError::internal_error(format!(
                "Function '{func_name}' is not a const function"
            )))
        }
    }

    fn eval_block(&mut self, block: &Block) -> CompilerResult<ConstValue> {
        let mut last_value = ConstValue::Int(0);

        for stmt in &block.statements {
            match stmt {
                Statement::Return { value } => {
                    if let Some(expr) = value {
                        return self.eval_expression(expr);
                    } else {
                        return Ok(ConstValue::Int(0));
                    }
                }
                Statement::Expression(expr) => {
                    last_value = self.eval_expression(expr)?;
                }
                Statement::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let cond_val = self.eval_expression(condition)?;
                    match cond_val {
                        ConstValue::Bool(true) => {
                            last_value = self.eval_block(then_branch)?;
                        }
                        ConstValue::Bool(false) => {
                            if let Some(else_block) = else_branch {
                                last_value = self.eval_block(else_block)?;
                            }
                        }
                        _ => {
                            return Err(CompilerError::internal_error(
                                "If condition must be a boolean".to_string(),
                            ))
                        }
                    }
                }
                _ => {
                    return Err(CompilerError::internal_error(
                        "Statement not supported in const evaluation".to_string(),
                    ))
                }
            }
        }

        Ok(last_value)
    }

    /// Validate a static assertion
    pub fn validate_static_assert(
        &mut self,
        condition: &Expression,
        message: &str,
    ) -> CompilerResult<()> {
        let result = self.eval_expression(condition)?;
        match result {
            ConstValue::Bool(true) => Ok(()),
            ConstValue::Bool(false) => Err(CompilerError::internal_error(format!(
                "Static assertion failed: {message}"
            ))),
            _ => Err(CompilerError::internal_error(
                "Static assertion condition must be a boolean".to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::Operator;
    use crate::parser::ast::Block;

    fn int(v: i64) -> Expression {
        Expression::IntLiteral(v)
    }
    fn boolv(v: bool) -> Expression {
        Expression::BoolLiteral(v)
    }
    fn binop(l: Expression, op: Operator, r: Expression) -> Expression {
        Expression::Binary {
            left: Box::new(l),
            operator: op,
            right: Box::new(r),
        }
    }
    fn unop(op: Operator, e: Expression) -> Expression {
        Expression::Unary {
            operator: op,
            operand: Box::new(e),
        }
    }

    #[test]
    fn test_default() {
        let e = ConstEvaluator::default();
        assert!(e.const_functions.is_empty());
        assert!(e.const_values.is_empty());
    }

    #[test]
    fn test_eval_int_literal() {
        let mut e = ConstEvaluator::new();
        assert_eq!(e.eval_expression(&int(42)).unwrap(), ConstValue::Int(42));
    }

    #[test]
    fn test_eval_float_literal() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&Expression::FloatLiteral(std::f64::consts::PI))
                .unwrap(),
            ConstValue::Float(std::f64::consts::PI)
        );
    }

    #[test]
    fn test_eval_bool_literal() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&boolv(true)).unwrap(),
            ConstValue::Bool(true)
        );
    }

    #[test]
    fn test_eval_string_literal() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&Expression::StringLiteral("hello".into()))
                .unwrap(),
            ConstValue::String("hello".into())
        );
    }

    #[test]
    fn test_eval_binary_arithmetic() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&binop(int(10), Operator::Plus, int(20)))
                .unwrap(),
            ConstValue::Int(30)
        );
        assert_eq!(
            e.eval_expression(&binop(int(20), Operator::Minus, int(7)))
                .unwrap(),
            ConstValue::Int(13)
        );
        assert_eq!(
            e.eval_expression(&binop(int(6), Operator::Star, int(7)))
                .unwrap(),
            ConstValue::Int(42)
        );
        assert_eq!(
            e.eval_expression(&binop(int(100), Operator::Slash, int(4)))
                .unwrap(),
            ConstValue::Int(25)
        );
        assert_eq!(
            e.eval_expression(&binop(int(10), Operator::Percent, int(3)))
                .unwrap(),
            ConstValue::Int(1)
        );
    }

    #[test]
    fn test_eval_division_by_zero() {
        let mut e = ConstEvaluator::new();
        assert!(e
            .eval_expression(&binop(int(10), Operator::Slash, int(0)))
            .is_err());
    }

    #[test]
    fn test_eval_comparison_ops() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&binop(int(10), Operator::Equal, int(10)))
                .unwrap(),
            ConstValue::Bool(true)
        );
        assert_eq!(
            e.eval_expression(&binop(int(10), Operator::Equal, int(5)))
                .unwrap(),
            ConstValue::Bool(false)
        );
        assert_eq!(
            e.eval_expression(&binop(int(10), Operator::NotEqual, int(5)))
                .unwrap(),
            ConstValue::Bool(true)
        );
        assert_eq!(
            e.eval_expression(&binop(int(5), Operator::Less, int(10)))
                .unwrap(),
            ConstValue::Bool(true)
        );
        assert_eq!(
            e.eval_expression(&binop(int(5), Operator::LessEqual, int(5)))
                .unwrap(),
            ConstValue::Bool(true)
        );
        assert_eq!(
            e.eval_expression(&binop(int(10), Operator::Greater, int(5)))
                .unwrap(),
            ConstValue::Bool(true)
        );
        assert_eq!(
            e.eval_expression(&binop(int(5), Operator::GreaterEqual, int(5)))
                .unwrap(),
            ConstValue::Bool(true)
        );
    }

    #[test]
    fn test_eval_bitwise_ops() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&binop(int(0xFF), Operator::BitAnd, int(0x0F)))
                .unwrap(),
            ConstValue::Int(0x0F)
        );
        assert_eq!(
            e.eval_expression(&binop(int(0xF0), Operator::BitOr, int(0x0F)))
                .unwrap(),
            ConstValue::Int(0xFF)
        );
        assert_eq!(
            e.eval_expression(&binop(int(0xFF), Operator::BitXor, int(0x0F)))
                .unwrap(),
            ConstValue::Int(0xF0)
        );
        assert_eq!(
            e.eval_expression(&binop(int(1), Operator::LeftShift, int(4)))
                .unwrap(),
            ConstValue::Int(16)
        );
        assert_eq!(
            e.eval_expression(&binop(int(16), Operator::RightShift, int(2)))
                .unwrap(),
            ConstValue::Int(4)
        );
    }

    #[test]
    fn test_eval_unsupported_int_op() {
        let mut e = ConstEvaluator::new();
        assert!(e
            .eval_expression(&binop(int(1), Operator::Assign, int(2)))
            .is_err());
    }

    #[test]
    fn test_eval_boolean_ops() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&binop(boolv(true), Operator::And, boolv(false)))
                .unwrap(),
            ConstValue::Bool(false)
        );
        assert_eq!(
            e.eval_expression(&binop(boolv(true), Operator::Or, boolv(false)))
                .unwrap(),
            ConstValue::Bool(true)
        );
        assert_eq!(
            e.eval_expression(&binop(boolv(true), Operator::Equal, boolv(true)))
                .unwrap(),
            ConstValue::Bool(true)
        );
        assert_eq!(
            e.eval_expression(&binop(boolv(true), Operator::NotEqual, boolv(false)))
                .unwrap(),
            ConstValue::Bool(true)
        );
    }

    #[test]
    fn test_eval_unsupported_bool_op() {
        let mut e = ConstEvaluator::new();
        assert!(e
            .eval_expression(&binop(boolv(true), Operator::Plus, boolv(false)))
            .is_err());
    }

    #[test]
    fn test_eval_type_mismatch_binary() {
        let mut e = ConstEvaluator::new();
        assert!(e
            .eval_expression(&binop(int(1), Operator::Plus, boolv(true)))
            .is_err());
    }

    #[test]
    fn test_eval_unary_negate() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&unop(Operator::Minus, int(42))).unwrap(),
            ConstValue::Int(-42)
        );
    }

    #[test]
    fn test_eval_unary_not() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&unop(Operator::Not, boolv(true)))
                .unwrap(),
            ConstValue::Bool(false)
        );
    }

    #[test]
    fn test_eval_unary_bitnot() {
        let mut e = ConstEvaluator::new();
        assert_eq!(
            e.eval_expression(&unop(Operator::BitNot, int(0))).unwrap(),
            ConstValue::Int(!0i64)
        );
    }

    #[test]
    fn test_eval_unsupported_unary() {
        let mut e = ConstEvaluator::new();
        assert!(e
            .eval_expression(&unop(Operator::Plus, boolv(true)))
            .is_err());
    }

    #[test]
    fn test_eval_identifier() {
        let mut e = ConstEvaluator::new();
        e.const_values.insert("X".into(), ConstValue::Int(100));
        assert_eq!(
            e.eval_expression(&Expression::Identifier("X".into()))
                .unwrap(),
            ConstValue::Int(100)
        );
    }

    #[test]
    fn test_eval_identifier_not_found() {
        let mut e = ConstEvaluator::new();
        assert!(e
            .eval_expression(&Expression::Identifier("MISSING".into()))
            .is_err());
    }

    #[test]
    fn test_eval_unsupported_expression() {
        let mut e = ConstEvaluator::new();
        assert!(e.eval_expression(&Expression::NullLiteral).is_err());
    }

    #[test]
    fn test_eval_const_function_call() {
        let mut e = ConstEvaluator::new();
        // Register: const fn add(a, b) { return a + b; }
        e.register_const_function(
            "add".into(),
            vec!["a".into(), "b".into()],
            Block {
                statements: vec![Statement::Return {
                    value: Some(binop(
                        Expression::Identifier("a".into()),
                        Operator::Plus,
                        Expression::Identifier("b".into()),
                    )),
                }],
            },
        );
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("add".into())),
            type_args: None,
            arguments: vec![int(10), int(32)],
        };
        assert_eq!(e.eval_expression(&call).unwrap(), ConstValue::Int(42));
    }

    #[test]
    fn test_eval_const_function_wrong_arity() {
        let mut e = ConstEvaluator::new();
        e.register_const_function("f".into(), vec!["a".into()], Block { statements: vec![] });
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("f".into())),
            type_args: None,
            arguments: vec![int(1), int(2)],
        };
        assert!(e.eval_expression(&call).is_err());
    }

    #[test]
    fn test_eval_unknown_function() {
        let mut e = ConstEvaluator::new();
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("unknown".into())),
            type_args: None,
            arguments: vec![],
        };
        assert!(e.eval_expression(&call).is_err());
    }

    #[test]
    fn test_eval_indirect_call_error() {
        let mut e = ConstEvaluator::new();
        let call = Expression::Call {
            callee: Box::new(int(42)),
            type_args: None,
            arguments: vec![],
        };
        assert!(e.eval_expression(&call).is_err());
    }

    #[test]
    fn test_eval_block_return_no_value() {
        let mut e = ConstEvaluator::new();
        e.register_const_function(
            "nop".into(),
            vec![],
            Block {
                statements: vec![Statement::Return { value: None }],
            },
        );
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("nop".into())),
            type_args: None,
            arguments: vec![],
        };
        assert_eq!(e.eval_expression(&call).unwrap(), ConstValue::Int(0));
    }

    #[test]
    fn test_eval_block_expression_stmt() {
        let mut e = ConstEvaluator::new();
        e.register_const_function(
            "f".into(),
            vec![],
            Block {
                statements: vec![Statement::Expression(int(99))],
            },
        );
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("f".into())),
            type_args: None,
            arguments: vec![],
        };
        assert_eq!(e.eval_expression(&call).unwrap(), ConstValue::Int(99));
    }

    #[test]
    fn test_eval_block_if_true() {
        let mut e = ConstEvaluator::new();
        e.register_const_function(
            "f".into(),
            vec![],
            Block {
                statements: vec![Statement::If {
                    condition: boolv(true),
                    then_branch: Block {
                        statements: vec![Statement::Return {
                            value: Some(int(1)),
                        }],
                    },
                    else_branch: Some(Block {
                        statements: vec![Statement::Return {
                            value: Some(int(2)),
                        }],
                    }),
                }],
            },
        );
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("f".into())),
            type_args: None,
            arguments: vec![],
        };
        assert_eq!(e.eval_expression(&call).unwrap(), ConstValue::Int(1));
    }

    #[test]
    fn test_eval_block_if_false_with_else() {
        let mut e = ConstEvaluator::new();
        e.register_const_function(
            "f".into(),
            vec![],
            Block {
                statements: vec![Statement::If {
                    condition: boolv(false),
                    then_branch: Block {
                        statements: vec![Statement::Return {
                            value: Some(int(1)),
                        }],
                    },
                    else_branch: Some(Block {
                        statements: vec![Statement::Return {
                            value: Some(int(2)),
                        }],
                    }),
                }],
            },
        );
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("f".into())),
            type_args: None,
            arguments: vec![],
        };
        assert_eq!(e.eval_expression(&call).unwrap(), ConstValue::Int(2));
    }

    #[test]
    fn test_eval_block_if_false_no_else() {
        let mut e = ConstEvaluator::new();
        e.register_const_function(
            "f".into(),
            vec![],
            Block {
                statements: vec![
                    Statement::If {
                        condition: boolv(false),
                        then_branch: Block {
                            statements: vec![Statement::Return {
                                value: Some(int(99)),
                            }],
                        },
                        else_branch: None,
                    },
                    Statement::Expression(int(0)),
                ],
            },
        );
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("f".into())),
            type_args: None,
            arguments: vec![],
        };
        assert_eq!(e.eval_expression(&call).unwrap(), ConstValue::Int(0));
    }

    #[test]
    fn test_eval_block_if_non_bool_condition() {
        let mut e = ConstEvaluator::new();
        e.register_const_function(
            "f".into(),
            vec![],
            Block {
                statements: vec![Statement::If {
                    condition: int(42),
                    then_branch: Block { statements: vec![] },
                    else_branch: None,
                }],
            },
        );
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("f".into())),
            type_args: None,
            arguments: vec![],
        };
        assert!(e.eval_expression(&call).is_err());
    }

    #[test]
    fn test_eval_block_unsupported_stmt() {
        let mut e = ConstEvaluator::new();
        e.register_const_function(
            "f".into(),
            vec![],
            Block {
                statements: vec![Statement::Break],
            },
        );
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("f".into())),
            type_args: None,
            arguments: vec![],
        };
        assert!(e.eval_expression(&call).is_err());
    }

    #[test]
    fn test_static_assert_pass() {
        let mut e = ConstEvaluator::new();
        assert!(e
            .validate_static_assert(&boolv(true), "should pass")
            .is_ok());
    }

    #[test]
    fn test_static_assert_fail() {
        let mut e = ConstEvaluator::new();
        assert!(e
            .validate_static_assert(&boolv(false), "should fail")
            .is_err());
    }

    #[test]
    fn test_static_assert_non_bool() {
        let mut e = ConstEvaluator::new();
        assert!(e.validate_static_assert(&int(42), "not a bool").is_err());
    }

    #[test]
    fn test_const_function_preserves_outer_scope() {
        let mut e = ConstEvaluator::new();
        e.const_values.insert("OUTER".into(), ConstValue::Int(999));
        e.register_const_function(
            "f".into(),
            vec!["x".into()],
            Block {
                statements: vec![Statement::Return {
                    value: Some(Expression::Identifier("x".into())),
                }],
            },
        );
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("f".into())),
            type_args: None,
            arguments: vec![int(42)],
        };
        assert_eq!(e.eval_expression(&call).unwrap(), ConstValue::Int(42));
        // Outer scope should be preserved
        assert_eq!(e.const_values.get("OUTER").unwrap(), &ConstValue::Int(999));
    }
}
