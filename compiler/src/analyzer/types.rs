use crate::parser::ast::{AssociatedType, TraitMethod, Type};
use std::collections::HashMap;

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
    /// Enum definitions
    enums: HashMap<String, EnumType>,
    /// Trait definitions
    traits: HashMap<String, TraitType>,
    /// Trait implementations: (trait_name, type_name) -> impl
    trait_impls: HashMap<(String, String), TraitImpl>,
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
            enums: HashMap::new(),
            traits: HashMap::new(),
            trait_impls: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new child environment with this as parent.
    pub fn child(&self) -> Self {
        Self {
            variables: HashMap::new(),
            functions: self.functions.clone(),
            structs: self.structs.clone(),
            enums: self.enums.clone(),
            traits: self.traits.clone(),
            trait_impls: self.trait_impls.clone(),
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

    pub fn function_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.functions.keys().cloned().collect();
        if let Some(parent) = &self.parent {
            names.extend(parent.function_names());
        }
        names.sort();
        names.dedup();
        names
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
    #[allow(dead_code)]
    pub fn has_variable_in_scope(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Define an enum in the current scope.
    pub fn define_enum(&mut self, name: String, enum_type: EnumType) {
        self.enums.insert(name, enum_type);
    }

    /// Look up an enum definition.
    pub fn lookup_enum(&self, name: &str) -> Option<EnumType> {
        if let Some(enum_type) = self.enums.get(name) {
            Some(enum_type.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_enum(name)
        } else {
            None
        }
    }

    /// Define a trait in the current scope.
    pub fn define_trait(&mut self, name: String, trait_type: TraitType) {
        self.traits.insert(name, trait_type);
    }

    /// Look up a trait definition.
    pub fn lookup_trait(&self, name: &str) -> Option<TraitType> {
        if let Some(trait_type) = self.traits.get(name) {
            Some(trait_type.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_trait(name)
        } else {
            None
        }
    }

    /// Define a trait implementation.
    pub fn define_trait_impl(&mut self, trait_name: String, type_name: String, impl_: TraitImpl) {
        self.trait_impls.insert((trait_name, type_name), impl_);
    }

    /// Look up a trait implementation.
    #[allow(dead_code)]
    pub fn lookup_trait_impl(&self, trait_name: &str, type_name: &str) -> Option<TraitImpl> {
        let key = (trait_name.to_string(), type_name.to_string());
        if let Some(impl_) = self.trait_impls.get(&key) {
            Some(impl_.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_trait_impl(trait_name, type_name)
        } else {
            None
        }
    }

    /// Check if a type implements a trait.
    #[allow(dead_code)]
    pub fn type_implements_trait(&self, type_name: &str, trait_name: &str) -> bool {
        self.lookup_trait_impl(trait_name, type_name).is_some()
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
            enums: self.enums.clone(),
            traits: self.traits.clone(),
            trait_impls: self.trait_impls.clone(),
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
    #[allow(dead_code)]
    pub fn has_field(&self, name: &str) -> bool {
        self.fields.contains_key(name)
    }
}

/// Enum type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumType {
    /// Enum name
    pub name: String,
    /// Variants: (variant_name, tag_value, optional_payload)
    pub variants: Vec<(String, u32, Option<crate::parser::ast::EnumVariantPayload>)>,
}

impl EnumType {
    /// Create a new enum type.
    pub fn new(
        name: String,
        variants: Vec<(String, Option<crate::parser::ast::EnumVariantPayload>)>,
    ) -> Self {
        let variants_with_tags: Vec<_> = variants
            .into_iter()
            .enumerate()
            .map(|(i, (name, payload))| (name, i as u32, payload))
            .collect();
        Self {
            name,
            variants: variants_with_tags,
        }
    }

    /// Get the tag value for a variant.
    #[allow(dead_code)]
    pub fn get_variant_tag(&self, variant_name: &str) -> Option<u32> {
        self.variants
            .iter()
            .find(|(name, _, _)| name == variant_name)
            .map(|(_, tag, _)| *tag)
    }

    /// Get the payload types for a variant.
    pub fn get_variant_payload(
        &self,
        variant_name: &str,
    ) -> Option<Option<crate::parser::ast::EnumVariantPayload>> {
        self.variants
            .iter()
            .find(|(name, _, _)| name == variant_name)
            .map(|(_, _, payload)| payload.clone())
    }

    /// Check if a variant exists.
    pub fn has_variant(&self, variant_name: &str) -> bool {
        self.variants
            .iter()
            .any(|(name, _, _)| name == variant_name)
    }
}

/// Trait type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitType {
    /// Trait name
    pub name: String,
    /// Generic parameters
    pub generic_params: Vec<String>,
    /// Super traits (trait inheritance)
    pub super_traits: Vec<String>,
    /// Trait methods
    pub methods: Vec<TraitMethod>,
    /// Associated types
    pub associated_types: Vec<AssociatedType>,
}

impl TraitType {
    /// Create a new trait type.
    pub fn new(
        name: String,
        generic_params: Vec<String>,
        super_traits: Vec<String>,
        methods: Vec<TraitMethod>,
        associated_types: Vec<AssociatedType>,
    ) -> Self {
        Self {
            name,
            generic_params,
            super_traits,
            methods,
            associated_types,
        }
    }

    /// Get a method by name.
    pub fn get_method(&self, method_name: &str) -> Option<&TraitMethod> {
        self.methods.iter().find(|m| m.name == method_name)
    }

    /// Check if a method exists.
    #[allow(dead_code)]
    pub fn has_method(&self, method_name: &str) -> bool {
        self.methods.iter().any(|m| m.name == method_name)
    }

    /// Get an associated type by name.
    #[allow(dead_code)]
    pub fn get_associated_type(&self, type_name: &str) -> Option<&AssociatedType> {
        self.associated_types.iter().find(|t| t.name == type_name)
    }
}

/// Trait implementation.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitImpl {
    /// Trait name
    pub trait_name: String,
    /// Type implementing the trait
    pub type_name: String,
    /// Generic parameters
    pub generic_params: Vec<String>,
    /// Method implementations
    pub methods: HashMap<String, FunctionType>,
}

impl TraitImpl {
    /// Create a new trait implementation.
    pub fn new(
        trait_name: String,
        type_name: String,
        generic_params: Vec<String>,
        methods: HashMap<String, FunctionType>,
    ) -> Self {
        Self {
            trait_name,
            type_name,
            generic_params,
            methods,
        }
    }

    /// Get a method implementation.
    #[allow(dead_code)]
    pub fn get_method(&self, method_name: &str) -> Option<&FunctionType> {
        self.methods.get(method_name)
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
        let func_type = FunctionType::new(vec![Type::Int, Type::Int], Type::Int, false);
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
