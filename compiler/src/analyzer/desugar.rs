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

    #[test]
    fn test_desugar_empty_program() {
        let mut desugar = AstDesugar::new();
        let mut program = Program { statements: vec![] };
        assert!(desugar.desugar_program(&mut program).is_ok());
    }
}
