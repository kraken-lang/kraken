use std::collections::HashMap;
use crate::parser::ast::Type;

/// Type environment for tracking variable and function types.
/// 
/// Maintains symbol tables for type checking and inference.
pub struct TypeEnvironment {
    /// Variable types in current scope
    variables: HashMap<String, Type>,
    /// Function signatures
    functions: HashMap<String, FunctionType>,
    /// Struct definitions
    structs: HashMap<String, StructType>,
    /// Parent scope for nested environments
    parent: Option<Box<TypeEnvironment>>,
}

impl TypeEnvironment {
    /// Create a new empty type environment.
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new child environment with this as parent.
    pub fn child(&self) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            parent: Some(Box::new(self.clone())),
        }
    }

    /// Define a variable in the current scope.
    /// 
    /// # Arguments
    /// * `name` - Variable name
    /// * `var_type` - Variable type
    pub fn define_variable(&mut self, name: String, var_type: Type) {
        self.variables.insert(name, var_type);
    }

    /// Look up a variable type.
    /// 
    /// # Arguments
    /// * `name` - Variable name
    /// 
    /// # Returns
    /// The variable's type if found
    pub fn lookup_variable(&self, name: &str) -> Option<Type> {
        if let Some(var_type) = self.variables.get(name) {
            Some(var_type.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_variable(name)
        } else {
            None
        }
    }

    /// Define a function in the current scope.
    /// 
    /// # Arguments
    /// * `name` - Function name
    /// * `func_type` - Function type signature
    pub fn define_function(&mut self, name: String, func_type: FunctionType) {
        self.functions.insert(name, func_type);
    }

    /// Look up a function type.
    /// 
    /// # Arguments
    /// * `name` - Function name
    /// 
    /// # Returns
    /// The function's type signature if found
    pub fn lookup_function(&self, name: &str) -> Option<FunctionType> {
        if let Some(func_type) = self.functions.get(name) {
            Some(func_type.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_function(name)
        } else {
            None
        }
    }

    /// Define a struct in the current scope.
    /// 
    /// # Arguments
    /// * `name` - Struct name
    /// * `struct_type` - Struct definition
    pub fn define_struct(&mut self, name: String, struct_type: StructType) {
        self.structs.insert(name, struct_type);
    }

    /// Look up a struct definition.
    /// 
    /// # Arguments
    /// * `name` - Struct name
    /// 
    /// # Returns
    /// The struct definition if found
    pub fn lookup_struct(&self, name: &str) -> Option<StructType> {
        if let Some(struct_type) = self.structs.get(name) {
            Some(struct_type.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_struct(name)
        } else {
            None
        }
    }

    /// Check if a variable exists in the current scope (not parent scopes).
    pub fn has_variable_in_scope(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for TypeEnvironment {
    fn clone(&self) -> Self {
        Self {
            variables: self.variables.clone(),
            functions: self.functions.clone(),
            structs: self.structs.clone(),
            parent: self.parent.clone(),
        }
    }
}

/// Function type signature.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
    pub is_async: bool,
}

impl FunctionType {
    /// Create a new function type.
    pub fn new(parameter_types: Vec<Type>, return_type: Type, is_async: bool) -> Self {
        Self {
            parameter_types,
            return_type,
            is_async,
        }
    }
}

/// Struct type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    pub fields: HashMap<String, Type>,
}

impl StructType {
    /// Create a new struct type.
    pub fn new(fields: HashMap<String, Type>) -> Self {
        Self { fields }
    }

    /// Get the type of a field.
    pub fn get_field_type(&self, name: &str) -> Option<&Type> {
        self.fields.get(name)
    }

    /// Check if a field exists.
    pub fn has_field(&self, name: &str) -> bool {
        self.fields.contains_key(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variable_definition_and_lookup() {
        let mut env = TypeEnvironment::new();
        env.define_variable("x".to_string(), Type::Int);
        
        assert_eq!(env.lookup_variable("x"), Some(Type::Int));
        assert_eq!(env.lookup_variable("y"), None);
    }

    #[test]
    fn test_child_environment() {
        let mut parent = TypeEnvironment::new();
        parent.define_variable("x".to_string(), Type::Int);
        
        let mut child = parent.child();
        child.define_variable("y".to_string(), Type::Float);
        
        assert_eq!(child.lookup_variable("x"), Some(Type::Int));
        assert_eq!(child.lookup_variable("y"), Some(Type::Float));
        assert_eq!(parent.lookup_variable("y"), None);
    }

    #[test]
    fn test_function_definition() {
        let mut env = TypeEnvironment::new();
        let func_type = FunctionType::new(
            vec![Type::Int, Type::Int],
            Type::Int,
            false,
        );
        env.define_function("add".to_string(), func_type.clone());
        
        assert_eq!(env.lookup_function("add"), Some(func_type));
    }

    #[test]
    fn test_struct_definition() {
        let mut env = TypeEnvironment::new();
        let mut fields = HashMap::new();
        fields.insert("x".to_string(), Type::Int);
        fields.insert("y".to_string(), Type::Float);
        
        let struct_type = StructType::new(fields);
        env.define_struct("Point".to_string(), struct_type.clone());
        
        assert_eq!(env.lookup_struct("Point"), Some(struct_type));
    }
}
