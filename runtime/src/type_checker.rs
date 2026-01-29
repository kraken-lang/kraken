//! Type checker for bootstrap compiler.
//!
//! Provides type checking infrastructure for the self-hosted compiler.

use std::collections::HashMap;

/// Type representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Void,
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },
    Generic {
        name: String,
        constraints: Vec<String>,
    },
}

impl Type {
    /// Check if this type is compatible with another type.
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Void, Type::Void) => true,
            (
                Type::Function {
                    params: p1,
                    return_type: r1,
                },
                Type::Function {
                    params: p2,
                    return_type: r2,
                },
            ) => {
                p1.len() == p2.len()
                    && p1
                        .iter()
                        .zip(p2.iter())
                        .all(|(a, b)| a.is_compatible_with(b))
                    && r1.is_compatible_with(r2)
            }
            _ => false,
        }
    }

    /// Get the size of this type in bytes.
    pub fn size_bytes(&self) -> usize {
        match self {
            Type::Int => 8,
            Type::Float => 8,
            Type::Bool => 1,
            Type::String => 16, // Pointer + length
            Type::Void => 0,
            Type::Function { .. } => 8, // Function pointer
            Type::Struct { fields, .. } => fields.iter().map(|(_, t)| t.size_bytes()).sum(),
            Type::Generic { .. } => 8, // Generic placeholder
        }
    }

    /// Check if this is a numeric type.
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
    }

    /// Check if this is a primitive type.
    pub fn is_primitive(&self) -> bool {
        matches!(self, Type::Int | Type::Float | Type::Bool | Type::String)
    }
}

/// Type environment for tracking variable types.
#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    scopes: Vec<HashMap<String, Type>>,
}

impl TypeEnvironment {
    /// Create a new type environment.
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    /// Push a new scope.
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Pop the current scope.
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a variable in the current scope.
    pub fn define(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    /// Look up a variable's type.
    pub fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    /// Check if a variable is defined in the current scope.
    pub fn is_defined(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

/// Type checker for expressions and statements.
pub struct TypeChecker {
    env: TypeEnvironment,
    errors: Vec<String>,
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new() -> Self {
        Self {
            env: TypeEnvironment::new(),
            errors: Vec::new(),
        }
    }

    /// Get the type environment.
    pub fn env(&self) -> &TypeEnvironment {
        &self.env
    }

    /// Get type checking errors.
    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Add an error.
    pub fn add_error(&mut self, error: String) {
        self.errors.push(error);
    }

    /// Check binary operation type.
    pub fn check_binary_op(&mut self, op: &str, left: &Type, right: &Type) -> Result<Type, String> {
        match op {
            "+" | "-" | "*" | "/" | "%" => {
                if left.is_numeric() && right.is_numeric() && left.is_compatible_with(right) {
                    Ok(left.clone())
                } else {
                    Err(format!(
                        "Type mismatch in {op} operation: {left:?} and {right:?}"
                    ))
                }
            }
            "==" | "!=" | "<" | "<=" | ">" | ">=" => {
                if left.is_compatible_with(right) {
                    Ok(Type::Bool)
                } else {
                    Err(format!(
                        "Type mismatch in {op} comparison: {left:?} and {right:?}"
                    ))
                }
            }
            "&&" | "||" => {
                if matches!(left, Type::Bool) && matches!(right, Type::Bool) {
                    Ok(Type::Bool)
                } else {
                    Err(format!("Logical {op} requires boolean operands"))
                }
            }
            _ => Err(format!("Unknown binary operator: {op}")),
        }
    }

    /// Check unary operation type.
    pub fn check_unary_op(&mut self, op: &str, operand: &Type) -> Result<Type, String> {
        match op {
            "-" => {
                if operand.is_numeric() {
                    Ok(operand.clone())
                } else {
                    Err(format!("Unary - requires numeric operand, got {operand:?}"))
                }
            }
            "!" => {
                if matches!(operand, Type::Bool) {
                    Ok(Type::Bool)
                } else {
                    Err(format!(
                        "Logical ! requires boolean operand, got {operand:?}"
                    ))
                }
            }
            _ => Err(format!("Unknown unary operator: {op}")),
        }
    }

    /// Check function call type.
    pub fn check_function_call(&mut self, func_type: &Type, args: &[Type]) -> Result<Type, String> {
        match func_type {
            Type::Function {
                params,
                return_type,
            } => {
                if params.len() != args.len() {
                    return Err(format!(
                        "Function expects {} arguments, got {}",
                        params.len(),
                        args.len()
                    ));
                }

                for (i, (param, arg)) in params.iter().zip(args.iter()).enumerate() {
                    if !arg.is_compatible_with(param) {
                        return Err(format!(
                            "Argument {i} type mismatch: expected {param:?}, got {arg:?}"
                        ));
                    }
                }

                Ok((**return_type).clone())
            }
            _ => Err(format!("Cannot call non-function type: {func_type:?}")),
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_compatibility() {
        assert!(Type::Int.is_compatible_with(&Type::Int));
        assert!(Type::Float.is_compatible_with(&Type::Float));
        assert!(!Type::Int.is_compatible_with(&Type::Float));
    }

    #[test]
    fn test_type_size() {
        assert_eq!(Type::Int.size_bytes(), 8);
        assert_eq!(Type::Float.size_bytes(), 8);
        assert_eq!(Type::Bool.size_bytes(), 1);
        assert_eq!(Type::Void.size_bytes(), 0);
    }

    #[test]
    fn test_type_predicates() {
        assert!(Type::Int.is_numeric());
        assert!(Type::Float.is_numeric());
        assert!(!Type::Bool.is_numeric());

        assert!(Type::Int.is_primitive());
        assert!(Type::Bool.is_primitive());
    }

    #[test]
    fn test_type_environment() {
        let mut env = TypeEnvironment::new();
        env.define("x".to_string(), Type::Int);
        assert_eq!(env.lookup("x"), Some(&Type::Int));
        assert!(env.is_defined("x"));
        assert!(!env.is_defined("y"));
    }

    #[test]
    fn test_type_environment_scopes() {
        let mut env = TypeEnvironment::new();
        env.define("x".to_string(), Type::Int);

        env.push_scope();
        env.define("y".to_string(), Type::Float);
        assert!(env.is_defined("x"));
        assert!(env.is_defined("y"));

        env.pop_scope();
        assert!(env.is_defined("x"));
        assert!(!env.is_defined("y"));
    }

    #[test]
    fn test_type_checker_binary_ops() {
        let mut checker = TypeChecker::new();

        let result = checker.check_binary_op("+", &Type::Int, &Type::Int);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Int);

        let result = checker.check_binary_op("==", &Type::Int, &Type::Int);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Bool);

        let result = checker.check_binary_op("+", &Type::Int, &Type::Float);
        assert!(result.is_err());
    }

    #[test]
    fn test_type_checker_unary_ops() {
        let mut checker = TypeChecker::new();

        let result = checker.check_unary_op("-", &Type::Int);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Int);

        let result = checker.check_unary_op("!", &Type::Bool);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Bool);

        let result = checker.check_unary_op("-", &Type::Bool);
        assert!(result.is_err());
    }

    #[test]
    fn test_type_checker_function_call() {
        let mut checker = TypeChecker::new();

        let func_type = Type::Function {
            params: vec![Type::Int, Type::Float],
            return_type: Box::new(Type::Bool),
        };

        let result = checker.check_function_call(&func_type, &[Type::Int, Type::Float]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::Bool);

        let result = checker.check_function_call(&func_type, &[Type::Int]);
        assert!(result.is_err());

        let result = checker.check_function_call(&func_type, &[Type::Bool, Type::Float]);
        assert!(result.is_err());
    }
}
