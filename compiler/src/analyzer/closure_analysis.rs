use crate::error::CompilerResult;
use crate::parser::ast::{Block, ClosureBody, Expression, Pattern, Statement};
use std::collections::HashSet;

/// A variable captured by a closure from an enclosing scope.
#[derive(Debug, Clone, PartialEq)]
pub struct CapturedVariable {
    pub name: String,
    pub is_mutable: bool,
    pub capture_by_value: bool,
}

/// The capture environment for a closure, tracking all captured variables.
#[derive(Debug, Clone)]
pub struct ClosureEnvironment {
    pub captured_vars: Vec<CapturedVariable>,
    pub is_move: bool,
}

impl ClosureEnvironment {
    /// Create a new closure environment.
    pub fn new(is_move: bool) -> Self {
        Self {
            captured_vars: Vec::new(),
            is_move,
        }
    }

    /// Add a captured variable to the environment (deduplicates by name).
    pub fn add_capture(&mut self, var: CapturedVariable) {
        if !self.captured_vars.iter().any(|v| v.name == var.name) {
            self.captured_vars.push(var);
        }
    }
}

/// Analyzes closure bodies to determine which variables are captured from enclosing scopes.
pub struct ClosureAnalyzer {
    local_vars: Vec<HashSet<String>>,
}

impl Default for ClosureAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl ClosureAnalyzer {
    /// Create a new closure analyzer with an empty root scope.
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

    fn _is_local(&self, name: &str) -> bool {
        self.local_vars.iter().any(|scope| scope.contains(name))
    }

    fn _is_in_outer_scopes(&self, name: &str) -> bool {
        // Check all scopes except the last one (current closure scope)
        if self.local_vars.len() <= 1 {
            return false;
        }
        self.local_vars[..self.local_vars.len() - 1]
            .iter()
            .any(|scope| scope.contains(name))
    }

    /// Analyze a closure body and return its capture environment.
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
            let is_param = self
                .local_vars
                .last()
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
            Expression::Call {
                callee, arguments, ..
            } => {
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
            Expression::Closure { body, .. } => match body {
                ClosureBody::Expression(expr) => {
                    self.collect_referenced_vars(expr, vars);
                }
                ClosureBody::Block(block) => {
                    self.collect_block_referenced_vars(block, vars);
                }
            },
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
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
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
            Statement::For {
                condition, body, ..
            } => {
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
            Statement::Unsafe { block } => {
                for stmt in &block.statements {
                    self.collect_statement_referenced_vars(stmt, vars);
                }
            }
            Statement::FunctionDeclaration { .. }
            | Statement::StructDeclaration { .. }
            | Statement::EnumDeclaration { .. }
            | Statement::UnionDeclaration { .. }
            | Statement::ClassDeclaration { .. }
            | Statement::InterfaceDeclaration { .. }
            | Statement::Module { .. }
            | Statement::Import { .. }
            | Statement::TypeAlias { .. }
            | Statement::ImplBlock { .. }
            | Statement::TraitDeclaration { .. }
            | Statement::TraitImpl { .. }
            | Statement::MacroDeclaration { .. }
            | Statement::ConstFunctionDeclaration { .. }
            | Statement::StaticAssert { .. }
            | Statement::Attribute { .. } => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::Operator;
    use crate::parser::ast::{MatchArm, Parameter, Type};

    fn ident(s: &str) -> Expression {
        Expression::Identifier(s.to_string())
    }
    fn int_lit(v: i64) -> Expression {
        Expression::IntLiteral(v)
    }
    fn block(stmts: Vec<Statement>) -> Block {
        Block { statements: stmts }
    }
    fn param(name: &str) -> Parameter {
        Parameter {
            pattern: Pattern::Identifier(name.to_string()),
            param_type: Type::Int,
            is_reference: false,
        }
    }

    // --- ClosureEnvironment tests ---

    #[test]
    fn test_env_new() {
        let env = ClosureEnvironment::new(false);
        assert!(env.captured_vars.is_empty());
        assert!(!env.is_move);
    }

    #[test]
    fn test_env_add_capture_dedup() {
        let mut env = ClosureEnvironment::new(false);
        env.add_capture(CapturedVariable {
            name: "x".into(),
            is_mutable: false,
            capture_by_value: false,
        });
        env.add_capture(CapturedVariable {
            name: "x".into(),
            is_mutable: true,
            capture_by_value: true,
        });
        assert_eq!(env.captured_vars.len(), 1);
    }

    // --- ClosureAnalyzer tests ---

    #[test]
    fn test_default() {
        let a = ClosureAnalyzer::default();
        assert_eq!(a.local_vars.len(), 1);
    }

    #[test]
    fn test_is_local() {
        let mut a = ClosureAnalyzer::new();
        a.add_local("x".into());
        assert!(a._is_local("x"));
        assert!(!a._is_local("y"));
    }

    #[test]
    fn test_is_in_outer_scopes() {
        let mut a = ClosureAnalyzer::new();
        a.add_local("outer".into());
        a.push_scope();
        a.add_local("inner".into());
        assert!(a._is_in_outer_scopes("outer"));
        assert!(!a._is_in_outer_scopes("inner"));
        a.pop_scope();
    }

    #[test]
    fn test_is_in_outer_scopes_single_scope() {
        let a = ClosureAnalyzer::new();
        assert!(!a._is_in_outer_scopes("x"));
    }

    #[test]
    fn test_simple_capture() {
        let mut a = ClosureAnalyzer::new();
        a.local_vars.first_mut().unwrap().insert("x".into());
        let env = a
            .analyze_closure(&[], &ClosureBody::Expression(Box::new(ident("x"))), false)
            .unwrap();
        assert_eq!(env.captured_vars.len(), 1);
        assert_eq!(env.captured_vars[0].name, "x");
        assert!(!env.captured_vars[0].capture_by_value);
    }

    #[test]
    fn test_move_capture() {
        let mut a = ClosureAnalyzer::new();
        a.local_vars.first_mut().unwrap().insert("x".into());
        let env = a
            .analyze_closure(&[], &ClosureBody::Expression(Box::new(ident("x"))), true)
            .unwrap();
        assert!(env.captured_vars[0].capture_by_value);
        assert!(env.is_move);
    }

    #[test]
    fn test_no_capture_for_param() {
        let mut a = ClosureAnalyzer::new();
        let env = a
            .analyze_closure(
                &[param("x")],
                &ClosureBody::Expression(Box::new(ident("x"))),
                false,
            )
            .unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_capture_binary_expr() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Binary {
            left: Box::new(ident("a")),
            operator: Operator::Plus,
            right: Box::new(ident("b")),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2);
    }

    #[test]
    fn test_capture_unary_expr() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Unary {
            operator: Operator::Minus,
            operand: Box::new(ident("x")),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_capture_call_expr() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Call {
            callee: Box::new(ident("f")),
            type_args: None,
            arguments: vec![ident("a"), ident("b")],
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.len() >= 3); // f, a, b
    }

    #[test]
    fn test_capture_index_expr() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Index {
            array: Box::new(ident("arr")),
            index: Box::new(ident("i")),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2);
    }

    #[test]
    fn test_capture_slice_expr() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Slice {
            array: Box::new(ident("arr")),
            start: Box::new(ident("s")),
            end: Box::new(ident("e")),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 3);
    }

    #[test]
    fn test_capture_member_access() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::MemberAccess {
            object: Box::new(ident("obj")),
            member: "field".into(),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
        assert_eq!(env.captured_vars[0].name, "obj");
    }

    #[test]
    fn test_capture_struct_literal() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::StructLiteral {
            name: "Pt".into(),
            type_args: None,
            fields: vec![("x".into(), ident("a")), ("y".into(), ident("b"))],
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2);
    }

    #[test]
    fn test_capture_array() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Array {
            elements: vec![ident("a"), ident("b")],
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2);
    }

    #[test]
    fn test_capture_tuple() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Tuple {
            elements: vec![ident("a"), ident("b")],
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2);
    }

    #[test]
    fn test_capture_tuple_index() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::TupleIndex {
            tuple: Box::new(ident("t")),
            index: 0,
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_capture_range() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Range {
            start: Box::new(ident("a")),
            end: Box::new(ident("b")),
            inclusive: false,
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2);
    }

    #[test]
    fn test_capture_try() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Try {
            expression: Box::new(ident("r")),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_capture_assignment() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Assignment {
            target: Box::new(ident("x")),
            value: Box::new(ident("y")),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2);
    }

    #[test]
    fn test_capture_reference_deref() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Reference {
            expression: Box::new(Expression::Dereference {
                expression: Box::new(ident("p")),
            }),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_capture_await() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Await {
            expression: Box::new(ident("fut")),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_capture_spawn() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Spawn {
            body: block(vec![Statement::Expression(ident("x"))]),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_capture_nested_closure() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Closure {
            parameters: vec![],
            return_type: None,
            body: ClosureBody::Expression(Box::new(ident("captured"))),
            is_move: false,
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_capture_nested_closure_block() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Closure {
            parameters: vec![],
            return_type: None,
            body: ClosureBody::Block(block(vec![Statement::Expression(ident("v"))])),
            is_move: false,
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_no_capture_for_literals() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Binary {
            left: Box::new(int_lit(1)),
            operator: Operator::Plus,
            right: Box::new(Expression::StringLiteral("hi".into())),
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_no_capture_for_null_bool_float() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::Tuple {
            elements: vec![
                Expression::NullLiteral,
                Expression::BoolLiteral(true),
                Expression::FloatLiteral(1.0),
            ],
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_no_capture_enum_variant() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Expression(Box::new(Expression::EnumVariant {
            enum_name: "Opt".into(),
            variant_name: "None".into(),
            payload: None,
        }));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    // --- Block body closure tests ---

    #[test]
    fn test_block_body_variable_decl() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::VariableDeclaration {
            pattern: Pattern::Identifier("y".into()),
            type_annotation: None,
            initializer: Some(ident("x")),
            is_mutable: false,
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
        assert_eq!(env.captured_vars[0].name, "x");
    }

    #[test]
    fn test_block_body_variable_decl_no_init() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::VariableDeclaration {
            pattern: Pattern::Identifier("y".into()),
            type_annotation: Some(Type::Int),
            initializer: None,
            is_mutable: false,
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_block_body_const_decl() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::ConstantDeclaration {
            name: "C".into(),
            type_annotation: None,
            initializer: ident("val"),
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_block_body_return() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::Return {
            value: Some(ident("x")),
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_block_body_return_void() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::Return { value: None }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_block_body_if_else() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::If {
            condition: ident("cond"),
            then_branch: block(vec![Statement::Expression(ident("a"))]),
            else_branch: Some(block(vec![Statement::Expression(ident("b"))])),
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 3); // cond, a, b
    }

    #[test]
    fn test_block_body_while() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::While {
            condition: ident("c"),
            body: block(vec![Statement::Expression(ident("x"))]),
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2);
    }

    #[test]
    fn test_block_body_for() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::For {
            initializer: None,
            condition: Some(ident("c")),
            increment: None,
            body: block(vec![Statement::Expression(ident("x"))]),
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2);
    }

    #[test]
    fn test_block_body_for_no_cond() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::For {
            initializer: None,
            condition: None,
            increment: None,
            body: block(vec![]),
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_block_body_for_in() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::ForIn {
            variable: "i".into(),
            iterable: ident("items"),
            body: block(vec![Statement::Expression(ident("acc"))]),
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.iter().any(|v| v.name == "items"));
        assert!(env.captured_vars.iter().any(|v| v.name == "acc"));
    }

    #[test]
    fn test_block_body_match() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::Match {
            expression: ident("val"),
            arms: vec![MatchArm {
                pattern: Pattern::Wildcard,
                guard: None,
                body: block(vec![Statement::Expression(ident("r"))]),
            }],
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 2); // val, r
    }

    #[test]
    fn test_block_body_break_continue() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::Break, Statement::Continue]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_block_body_defer() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::Defer {
            statement: Box::new(Statement::Expression(ident("cleanup"))),
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_block_body_unsafe() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![Statement::Unsafe {
            block: block(vec![Statement::Expression(ident("ptr"))]),
        }]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert_eq!(env.captured_vars.len(), 1);
    }

    #[test]
    fn test_block_body_skips_declarations() {
        let mut a = ClosureAnalyzer::new();
        let body = ClosureBody::Block(block(vec![
            Statement::FunctionDeclaration {
                name: "f".into(),
                generic_params: vec![],
                where_constraints: vec![],
                parameters: vec![],
                return_type: None,
                body: block(vec![]),
                is_async: false,
                is_unsafe: false,
                is_public: false,
                is_variadic: false,
            },
            Statement::StructDeclaration {
                name: "S".into(),
                generic_params: vec![],
                where_constraints: vec![],
                fields: vec![],
                is_public: false,
                repr: None,
            },
            Statement::Import {
                path: vec!["std".into()],
            },
            Statement::Module {
                path: vec!["mod".into()],
            },
        ]));
        let env = a.analyze_closure(&[], &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    // --- Pattern binding tests ---

    #[test]
    fn test_pattern_tuple_binding() {
        let mut a = ClosureAnalyzer::new();
        let params = vec![Parameter {
            pattern: Pattern::Tuple {
                patterns: vec![
                    Pattern::Identifier("a".into()),
                    Pattern::Identifier("b".into()),
                ],
            },
            param_type: Type::Int,
            is_reference: false,
        }];
        let body = ClosureBody::Expression(Box::new(Expression::Binary {
            left: Box::new(ident("a")),
            operator: Operator::Plus,
            right: Box::new(ident("b")),
        }));
        let env = a.analyze_closure(&params, &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_pattern_struct_binding() {
        let mut a = ClosureAnalyzer::new();
        let params = vec![Parameter {
            pattern: Pattern::Struct {
                struct_name: "Point".into(),
                fields: vec![
                    ("x".into(), Pattern::Identifier("px".into())),
                    ("y".into(), Pattern::Identifier("py".into())),
                ],
                partial: false,
            },
            param_type: Type::Int,
            is_reference: false,
        }];
        let body = ClosureBody::Expression(Box::new(ident("px")));
        let env = a.analyze_closure(&params, &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_pattern_enum_variant_binding() {
        let mut a = ClosureAnalyzer::new();
        let params = vec![Parameter {
            pattern: Pattern::EnumVariant {
                enum_name: "Opt".into(),
                variant_name: "Some".into(),
                bindings: vec!["val".into()],
            },
            param_type: Type::Int,
            is_reference: false,
        }];
        let body = ClosureBody::Expression(Box::new(ident("val")));
        let env = a.analyze_closure(&params, &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_pattern_or_binding() {
        let mut a = ClosureAnalyzer::new();
        let params = vec![Parameter {
            pattern: Pattern::Or {
                patterns: vec![
                    Pattern::Identifier("x".into()),
                    Pattern::Identifier("x".into()),
                ],
            },
            param_type: Type::Int,
            is_reference: false,
        }];
        let body = ClosureBody::Expression(Box::new(ident("x")));
        let env = a.analyze_closure(&params, &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }

    #[test]
    fn test_pattern_wildcard_literal_range() {
        let mut a = ClosureAnalyzer::new();
        let params = vec![Parameter {
            pattern: Pattern::Wildcard,
            param_type: Type::Int,
            is_reference: false,
        }];
        let body = ClosureBody::Expression(Box::new(int_lit(42)));
        let env = a.analyze_closure(&params, &body, false).unwrap();
        assert!(env.captured_vars.is_empty());
    }
}
