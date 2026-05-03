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
    fn test_default() {
        let c = DropChecker::default();
        assert_eq!(c.scope_stack.len(), 1);
    }

    #[test]
    fn test_new() {
        let c = DropChecker::new();
        assert!(!c.has_drop_impl("X"));
        assert_eq!(c.scope_stack.len(), 1);
    }

    #[test]
    fn test_register_drop_impl() {
        let mut c = DropChecker::new();
        c.register_drop_impl("Res".into(), true);
        assert!(c.has_drop_impl("Res"));
        assert!(!c.has_drop_impl("Other"));
    }

    #[test]
    fn test_register_drop_impl_custom_flag() {
        let mut c = DropChecker::new();
        c.register_drop_impl("A".into(), false);
        assert!(c.has_drop_impl("A"));
        assert!(!c.drop_impls["A"].has_custom_drop);
    }

    #[test]
    fn test_needs_drop_custom() {
        let mut c = DropChecker::new();
        c.register_drop_impl("Res".into(), true);
        assert!(c.needs_drop(&Type::Custom("Res".into())));
        assert!(!c.needs_drop(&Type::Custom("Other".into())));
    }

    #[test]
    fn test_needs_drop_primitives() {
        let c = DropChecker::new();
        assert!(!c.needs_drop(&Type::Int));
        assert!(!c.needs_drop(&Type::Float));
        assert!(!c.needs_drop(&Type::Bool));
        assert!(!c.needs_drop(&Type::String));
        assert!(!c.needs_drop(&Type::Str));
        assert!(!c.needs_drop(&Type::Void));
    }

    #[test]
    fn test_needs_drop_array() {
        let mut c = DropChecker::new();
        c.register_drop_impl("Res".into(), true);
        assert!(c.needs_drop(&Type::Array {
            element_type: Box::new(Type::Custom("Res".into())),
            size: None,
        }));
        assert!(!c.needs_drop(&Type::Array {
            element_type: Box::new(Type::Int),
            size: None,
        }));
    }

    #[test]
    fn test_needs_drop_reference() {
        let mut c = DropChecker::new();
        c.register_drop_impl("Res".into(), true);
        assert!(c.needs_drop(&Type::Reference {
            inner_type: Box::new(Type::Custom("Res".into())),
            is_mutable: false,
        }));
        assert!(!c.needs_drop(&Type::Reference {
            inner_type: Box::new(Type::Int),
            is_mutable: true,
        }));
    }

    #[test]
    fn test_needs_drop_tuple() {
        let mut c = DropChecker::new();
        c.register_drop_impl("Res".into(), true);
        assert!(c.needs_drop(&Type::Tuple {
            element_types: vec![Type::Int, Type::Custom("Res".into())],
        }));
        assert!(!c.needs_drop(&Type::Tuple {
            element_types: vec![Type::Int, Type::Bool],
        }));
    }

    #[test]
    fn test_scope_enter_exit() {
        let mut c = DropChecker::new();
        c.enter_scope();
        assert_eq!(c.scope_stack.len(), 2);
        c.register_variable("x".into(), Type::Int);
        let vars = c.exit_scope();
        assert_eq!(vars.len(), 1);
        assert_eq!(vars[0].name, "x");
        assert_eq!(c.scope_stack.len(), 1);
    }

    #[test]
    fn test_exit_scope_empty_stack() {
        let mut c = DropChecker::new();
        c.scope_stack.clear();
        let vars = c.exit_scope();
        assert!(vars.is_empty());
    }

    #[test]
    fn test_drop_order_reverse() {
        let mut c = DropChecker::new();
        c.register_drop_impl("R".into(), true);
        c.enter_scope();
        c.register_variable("a".into(), Type::Custom("R".into()));
        c.register_variable("b".into(), Type::Custom("R".into()));
        c.register_variable("c".into(), Type::Custom("R".into()));
        let vars = c.exit_scope();
        assert_eq!(vars[0].name, "c");
        assert_eq!(vars[1].name, "b");
        assert_eq!(vars[2].name, "a");
    }

    #[test]
    fn test_register_variable_needs_drop() {
        let mut c = DropChecker::new();
        c.register_drop_impl("R".into(), true);
        c.enter_scope();
        c.register_variable("droppable".into(), Type::Custom("R".into()));
        c.register_variable("plain".into(), Type::Int);
        let vars = c.exit_scope();
        assert!(vars[1].needs_drop); // droppable (reversed)
        assert!(!vars[0].needs_drop); // plain (reversed)
    }

    #[test]
    fn test_validate_drop_impl() {
        let mut c = DropChecker::new();
        c.register_drop_impl("R".into(), true);
        assert!(c.validate_drop_impl("R").is_ok());
        assert!(c.validate_drop_impl("Missing").is_err());
    }

    #[test]
    fn test_get_drop_variables() {
        let c = DropChecker::new();
        let vars = vec![
            VariableInfo {
                name: "x".into(),
                var_type: Type::Int,
                needs_drop: true,
            },
            VariableInfo {
                name: "y".into(),
                var_type: Type::Int,
                needs_drop: false,
            },
            VariableInfo {
                name: "z".into(),
                var_type: Type::Int,
                needs_drop: true,
            },
        ];
        let drop_vars = c.get_drop_variables(&vars);
        assert_eq!(drop_vars, vec!["x", "z"]);
    }

    #[test]
    fn test_get_drop_variables_none() {
        let c = DropChecker::new();
        let vars = vec![VariableInfo {
            name: "a".into(),
            var_type: Type::Int,
            needs_drop: false,
        }];
        assert!(c.get_drop_variables(&vars).is_empty());
    }

    #[test]
    fn test_nested_scopes() {
        let mut c = DropChecker::new();
        c.enter_scope();
        c.register_variable("outer".into(), Type::Int);
        c.enter_scope();
        c.register_variable("inner".into(), Type::Bool);
        let inner_vars = c.exit_scope();
        assert_eq!(inner_vars.len(), 1);
        assert_eq!(inner_vars[0].name, "inner");
        let outer_vars = c.exit_scope();
        assert_eq!(outer_vars.len(), 1);
        assert_eq!(outer_vars[0].name, "outer");
    }
}
