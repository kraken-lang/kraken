//! Compile-time constant evaluation for const functions and static assertions.

#![allow(dead_code)]

use crate::error::{CompilerError, CompilerResult};
use crate::parser::ast::{Block, Expression, Statement};
use std::collections::HashMap;

/// Constant value that can be evaluated at compile time
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

/// Constant evaluator for compile-time evaluation
#[allow(dead_code)]
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

    #[test]
    fn test_eval_int_literal() {
        let mut evaluator = ConstEvaluator::new();
        let expr = Expression::IntLiteral(42);
        let result = evaluator.eval_expression(&expr).unwrap();
        assert_eq!(result, ConstValue::Int(42));
    }

    #[test]
    fn test_eval_binary_add() {
        let mut evaluator = ConstEvaluator::new();
        let expr = Expression::Binary {
            left: Box::new(Expression::IntLiteral(10)),
            operator: crate::lexer::token::Operator::Plus,
            right: Box::new(Expression::IntLiteral(20)),
        };
        let result = evaluator.eval_expression(&expr).unwrap();
        assert_eq!(result, ConstValue::Int(30));
    }

    #[test]
    fn test_eval_comparison() {
        let mut evaluator = ConstEvaluator::new();
        let expr = Expression::Binary {
            left: Box::new(Expression::IntLiteral(10)),
            operator: crate::lexer::token::Operator::Greater,
            right: Box::new(Expression::IntLiteral(5)),
        };
        let result = evaluator.eval_expression(&expr).unwrap();
        assert_eq!(result, ConstValue::Bool(true));
    }
}
