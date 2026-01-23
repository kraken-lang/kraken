use crate::error::CompilerResult;
use crate::parser::ast::{Block, ClosureBody, Expression, Pattern, Statement};
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub struct CapturedVariable {
    pub name: String,
    pub is_mutable: bool,
    pub capture_by_value: bool,
}

#[derive(Debug, Clone)]
pub struct ClosureEnvironment {
    pub captured_vars: Vec<CapturedVariable>,
    #[allow(dead_code)]
    pub is_move: bool,
}

impl ClosureEnvironment {
    pub fn new(is_move: bool) -> Self {
        Self {
            captured_vars: Vec::new(),
            is_move,
        }
    }

    pub fn add_capture(&mut self, var: CapturedVariable) {
        if !self.captured_vars.iter().any(|v| v.name == var.name) {
            self.captured_vars.push(var);
        }
    }
}

pub struct ClosureAnalyzer {
    local_vars: Vec<HashSet<String>>,
}

impl Default for ClosureAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl ClosureAnalyzer {
    pub fn new() -> Self {
        Self {
            local_vars: vec![HashSet::new()],
        }
    }

    fn push_scope(&mut self) {
        self.local_vars.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.local_vars.pop();
    }

    fn add_local(&mut self, name: String) {
        if let Some(scope) = self.local_vars.last_mut() {
            scope.insert(name);
        }
    }

    #[allow(dead_code)]
    fn is_local(&self, name: &str) -> bool {
        self.local_vars.iter().any(|scope| scope.contains(name))
    }

    #[allow(dead_code)]
    fn is_in_outer_scopes(&self, name: &str) -> bool {
        // Check all scopes except the last one (current closure scope)
        if self.local_vars.len() <= 1 {
            return false;
        }
        self.local_vars[..self.local_vars.len() - 1]
            .iter()
            .any(|scope| scope.contains(name))
    }

    pub fn analyze_closure(
        &mut self,
        parameters: &[crate::parser::ast::Parameter],
        body: &ClosureBody,
        is_move: bool,
    ) -> CompilerResult<ClosureEnvironment> {
        let mut env = ClosureEnvironment::new(is_move);
        
        // Collect all referenced variables first
        let mut referenced_vars = HashSet::new();
        match body {
            ClosureBody::Expression(expr) => {
                self.collect_referenced_vars(expr, &mut referenced_vars);
            }
            ClosureBody::Block(block) => {
                self.collect_block_referenced_vars(block, &mut referenced_vars);
            }
        }
        
        // Now create closure scope with parameters
        self.push_scope();
        for param in parameters {
            self.add_pattern_bindings(&param.pattern);
        }
        
        // Determine which referenced vars are captures
        // A variable is captured if it's referenced but not a closure parameter
        for var_name in referenced_vars {
            // Check if it's in the current closure scope (i.e., a parameter)
            let is_param = self.local_vars.last()
                .map(|scope| scope.contains(&var_name))
                .unwrap_or(false);
            
            // If not a parameter, it's a capture
            if !is_param {
                env.add_capture(CapturedVariable {
                    name: var_name,
                    is_mutable: false,
                    capture_by_value: is_move,
                });
            }
        }
        
        self.pop_scope();
        
        Ok(env)
    }

    fn add_pattern_bindings(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Identifier(name) => {
                self.add_local(name.clone());
            }
            Pattern::Tuple { patterns } => {
                for elem in patterns {
                    self.add_pattern_bindings(elem);
                }
            }
            Pattern::Struct { fields, .. } => {
                for (_, field_pattern) in fields {
                    self.add_pattern_bindings(field_pattern);
                }
            }
            Pattern::EnumVariant { bindings, .. } => {
                for binding in bindings {
                    self.add_local(binding.clone());
                }
            }
            Pattern::Or { patterns } => {
                for pat in patterns {
                    self.add_pattern_bindings(pat);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Range { .. } => {}
        }
    }

    fn collect_referenced_vars(&self, expr: &Expression, vars: &mut HashSet<String>) {
        match expr {
            Expression::Identifier(name) => {
                vars.insert(name.clone());
            }
            Expression::Binary { left, right, .. } => {
                self.collect_referenced_vars(left, vars);
                self.collect_referenced_vars(right, vars);
            }
            Expression::Unary { operand, .. } => {
                self.collect_referenced_vars(operand, vars);
            }
            Expression::Call { callee, arguments, .. } => {
                self.collect_referenced_vars(callee, vars);
                for arg in arguments {
                    self.collect_referenced_vars(arg, vars);
                }
            }
            Expression::Index { array, index } => {
                self.collect_referenced_vars(array, vars);
                self.collect_referenced_vars(index, vars);
            }
            Expression::Slice { array, start, end } => {
                self.collect_referenced_vars(array, vars);
                self.collect_referenced_vars(start, vars);
                self.collect_referenced_vars(end, vars);
            }
            Expression::MemberAccess { object, .. } => {
                self.collect_referenced_vars(object, vars);
            }
            Expression::StructLiteral { fields, .. } => {
                for (_, field_expr) in fields {
                    self.collect_referenced_vars(field_expr, vars);
                }
            }
            Expression::Array { elements } => {
                for elem in elements {
                    self.collect_referenced_vars(elem, vars);
                }
            }
            Expression::Tuple { elements } => {
                for elem in elements {
                    self.collect_referenced_vars(elem, vars);
                }
            }
            Expression::TupleIndex { tuple, .. } => {
                self.collect_referenced_vars(tuple, vars);
            }
            Expression::Range { start, end, .. } => {
                self.collect_referenced_vars(start, vars);
                self.collect_referenced_vars(end, vars);
            }
            Expression::Try { expression } => {
                self.collect_referenced_vars(expression, vars);
            }
            Expression::Assignment { target, value } => {
                self.collect_referenced_vars(target, vars);
                self.collect_referenced_vars(value, vars);
            }
            Expression::Reference { expression } => {
                self.collect_referenced_vars(expression, vars);
            }
            Expression::Dereference { expression } => {
                self.collect_referenced_vars(expression, vars);
            }
            Expression::Await { expression } => {
                self.collect_referenced_vars(expression, vars);
            }
            Expression::Spawn { body } => {
                self.collect_block_referenced_vars(body, vars);
            }
            Expression::Closure { body, .. } => {
                match body {
                    ClosureBody::Expression(expr) => {
                        self.collect_referenced_vars(expr, vars);
                    }
                    ClosureBody::Block(block) => {
                        self.collect_block_referenced_vars(block, vars);
                    }
                }
            }
            Expression::IntLiteral(_)
            | Expression::FloatLiteral(_)
            | Expression::StringLiteral(_)
            | Expression::BoolLiteral(_)
            | Expression::NullLiteral
            | Expression::EnumVariant { .. } => {}
        }
    }

    fn collect_block_referenced_vars(&self, block: &Block, vars: &mut HashSet<String>) {
        for stmt in &block.statements {
            self.collect_statement_referenced_vars(stmt, vars);
        }
    }

    fn collect_statement_referenced_vars(&self, stmt: &Statement, vars: &mut HashSet<String>) {
        match stmt {
            Statement::Expression(expr) => {
                self.collect_referenced_vars(expr, vars);
            }
            Statement::VariableDeclaration { initializer, .. } => {
                if let Some(val) = initializer {
                    self.collect_referenced_vars(val, vars);
                }
            }
            Statement::ConstantDeclaration { initializer, .. } => {
                self.collect_referenced_vars(initializer, vars);
            }
            Statement::Return { value } => {
                if let Some(val) = value {
                    self.collect_referenced_vars(val, vars);
                }
            }
            Statement::If { condition, then_branch, else_branch } => {
                self.collect_referenced_vars(condition, vars);
                self.collect_block_referenced_vars(then_branch, vars);
                if let Some(else_blk) = else_branch {
                    self.collect_block_referenced_vars(else_blk, vars);
                }
            }
            Statement::While { condition, body } => {
                self.collect_referenced_vars(condition, vars);
                self.collect_block_referenced_vars(body, vars);
            }
            Statement::For { condition, body, .. } => {
                if let Some(cond) = condition {
                    self.collect_referenced_vars(cond, vars);
                }
                self.collect_block_referenced_vars(body, vars);
            }
            Statement::ForIn { iterable, body, .. } => {
                self.collect_referenced_vars(iterable, vars);
                self.collect_block_referenced_vars(body, vars);
            }
            Statement::Match { expression, arms } => {
                self.collect_referenced_vars(expression, vars);
                for arm in arms {
                    self.collect_block_referenced_vars(&arm.body, vars);
                }
            }
            Statement::Break | Statement::Continue => {}
            Statement::Defer { statement } => {
                self.collect_statement_referenced_vars(statement, vars);
            }
            Statement::FunctionDeclaration { .. }
            | Statement::StructDeclaration { .. }
            | Statement::EnumDeclaration { .. }
            | Statement::ClassDeclaration { .. }
            | Statement::InterfaceDeclaration { .. }
            | Statement::Module { .. }
            | Statement::Import { .. } => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{Parameter, Type};

    #[test]
    fn test_simple_capture() {
        let mut analyzer = ClosureAnalyzer::new();
        
        // Simulate outer scope with variable 'x'
        // The initial scope is for the function, add 'x' there
        if let Some(scope) = analyzer.local_vars.first_mut() {
            scope.insert("x".to_string());
        }
        
        let params = vec![];
        let body = ClosureBody::Expression(Box::new(Expression::Identifier("x".to_string())));
        
        let env = analyzer.analyze_closure(&params, &body, false).unwrap();
        
        assert_eq!(env.captured_vars.len(), 1);
        assert_eq!(env.captured_vars[0].name, "x");
        assert!(!env.captured_vars[0].capture_by_value);
    }

    #[test]
    fn test_move_capture() {
        let mut analyzer = ClosureAnalyzer::new();
        
        // Simulate outer scope with variable 'x'
        if let Some(scope) = analyzer.local_vars.first_mut() {
            scope.insert("x".to_string());
        }
        
        let params = vec![];
        let body = ClosureBody::Expression(Box::new(Expression::Identifier("x".to_string())));
        
        let env = analyzer.analyze_closure(&params, &body, true).unwrap();
        
        assert_eq!(env.captured_vars.len(), 1);
        assert_eq!(env.captured_vars[0].name, "x");
        assert!(env.captured_vars[0].capture_by_value);
    }

    #[test]
    fn test_no_capture_local_param() {
        let mut analyzer = ClosureAnalyzer::new();
        
        let params = vec![Parameter {
            pattern: Pattern::Identifier("x".to_string()),
            param_type: Type::Int,
            is_reference: false,
        }];
        let body = ClosureBody::Expression(Box::new(Expression::Identifier("x".to_string())));
        
        let env = analyzer.analyze_closure(&params, &body, false).unwrap();
        
        assert_eq!(env.captured_vars.len(), 0);
    }
}
