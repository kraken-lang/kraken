//! Drop trait implementation and RAII support for automatic resource cleanup.


use crate::error::{CompilerError, CompilerResult};
use crate::parser::ast::Type;
use std::collections::HashMap;

/// Drop checker for tracking Drop trait implementations and scope-based cleanup
pub struct DropChecker {
    /// Track which types implement Drop
    drop_impls: HashMap<String, DropImpl>,
    /// Track variables in current scope for drop order
    scope_stack: Vec<ScopeInfo>,
}

/// Information about a Drop trait implementation
#[derive(Debug, Clone)]
pub struct DropImpl {
    pub type_name: String,
    pub has_custom_drop: bool,
}

/// Information about a scope for drop order tracking
#[derive(Debug, Clone)]
pub struct ScopeInfo {
    pub variables: Vec<VariableInfo>,
}

/// Information about a variable for drop tracking
#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub var_type: Type,
    pub needs_drop: bool,
}

impl DropChecker {
    /// Create a new drop checker with an empty root scope.
    pub fn new() -> Self {
        Self {
            drop_impls: HashMap::new(),
            scope_stack: vec![ScopeInfo { variables: vec![] }],
        }
    }

    /// Register a Drop trait implementation for a type
    pub fn register_drop_impl(&mut self, type_name: String, has_custom: bool) {
        self.drop_impls.insert(
            type_name.clone(),
            DropImpl {
                type_name,
                has_custom_drop: has_custom,
            },
        );
    }

    /// Check if a type implements Drop
    pub fn has_drop_impl(&self, type_name: &str) -> bool {
        self.drop_impls.contains_key(type_name)
    }

    /// Check if a type needs drop (has Drop impl or contains types that do)
    pub fn needs_drop(&self, var_type: &Type) -> bool {
        match var_type {
            Type::Custom(name) => self.has_drop_impl(name),
            Type::Array { element_type, .. } => self.needs_drop(element_type),
            Type::Reference { inner_type, .. } => self.needs_drop(inner_type),
            Type::Tuple { element_types } => element_types.iter().any(|t| self.needs_drop(t)),
            _ => false,
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.scope_stack.push(ScopeInfo { variables: vec![] });
    }

    /// Exit a scope and return variables to drop in reverse order
    pub fn exit_scope(&mut self) -> Vec<VariableInfo> {
        if let Some(scope) = self.scope_stack.pop() {
            let mut vars = scope.variables;
            vars.reverse(); // Drop in reverse declaration order
            vars
        } else {
            vec![]
        }
    }

    /// Register a variable in the current scope
    pub fn register_variable(&mut self, name: String, var_type: Type) {
        let needs_drop = self.needs_drop(&var_type);
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.variables.push(VariableInfo {
                name,
                var_type,
                needs_drop,
            });
        }
    }

    /// Validate Drop trait implementation
    pub fn validate_drop_impl(&self, type_name: &str) -> CompilerResult<()> {
        if !self.has_drop_impl(type_name) {
            return Err(CompilerError::internal_error(format!(
                "Type '{type_name}' does not implement Drop"
            )));
        }
        Ok(())
    }

    /// Get variables that need drop calls for a scope
    pub fn get_drop_variables(&self, variables: &[VariableInfo]) -> Vec<String> {
        variables
            .iter()
            .filter(|v| v.needs_drop)
            .map(|v| v.name.clone())
            .collect()
    }
}

impl Default for DropChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_drop_checker_creation() {
        let checker = DropChecker::new();
        assert_eq!(checker.scope_stack.len(), 1);
        assert!(!checker.has_drop_impl("String"));
    }

    #[test]
    fn test_register_drop_impl() {
        let mut checker = DropChecker::new();
        checker.register_drop_impl("MyType".to_string(), true);
        assert!(checker.has_drop_impl("MyType"));
    }

    #[test]
    fn test_needs_drop() {
        let mut checker = DropChecker::new();
        checker.register_drop_impl("MyType".to_string(), true);

        assert!(checker.needs_drop(&Type::Custom("MyType".to_string())));
        assert!(!checker.needs_drop(&Type::Int));
        assert!(!checker.needs_drop(&Type::String));
    }

    #[test]
    fn test_scope_management() {
        let mut checker = DropChecker::new();
        checker.enter_scope();
        assert_eq!(checker.scope_stack.len(), 2);

        checker.register_variable("x".to_string(), Type::Int);
        checker.register_variable("y".to_string(), Type::String);

        let vars = checker.exit_scope();
        assert_eq!(vars.len(), 2);
        assert_eq!(vars[0].name, "y"); // Reverse order
        assert_eq!(vars[1].name, "x");
    }

    #[test]
    fn test_drop_order() {
        let mut checker = DropChecker::new();
        checker.register_drop_impl("Resource".to_string(), true);

        checker.enter_scope();
        checker.register_variable("a".to_string(), Type::Custom("Resource".to_string()));
        checker.register_variable("b".to_string(), Type::Custom("Resource".to_string()));
        checker.register_variable("c".to_string(), Type::Custom("Resource".to_string()));

        let vars = checker.exit_scope();
        assert_eq!(vars.len(), 3);
        assert_eq!(vars[0].name, "c"); // Dropped first (last declared)
        assert_eq!(vars[1].name, "b");
        assert_eq!(vars[2].name, "a"); // Dropped last (first declared)
    }

    #[test]
    fn test_get_drop_variables() {
        let mut checker = DropChecker::new();
        checker.register_drop_impl("Resource".to_string(), true);

        let variables = vec![
            VariableInfo {
                name: "x".to_string(),
                var_type: Type::Custom("Resource".to_string()),
                needs_drop: true,
            },
            VariableInfo {
                name: "y".to_string(),
                var_type: Type::Int,
                needs_drop: false,
            },
        ];

        let drop_vars = checker.get_drop_variables(&variables);
        assert_eq!(drop_vars.len(), 1); // Only x needs drop
        assert_eq!(drop_vars[0], "x");
    }
}
