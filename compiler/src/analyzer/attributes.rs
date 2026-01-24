//! Attribute processing and validation for Kraken compiler.

#![allow(dead_code)]

use crate::error::{CompilerError, CompilerResult};
use std::collections::HashMap;

/// Attribute information extracted from AST
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum AttributeValue {
    /// Simple flag attribute (e.g., #[inline])
    Flag,
    /// Attribute with arguments (e.g., #[derive(Clone, Debug)])
    Args(Vec<String>),
}

/// Attribute processor for managing function and type attributes
#[allow(dead_code)]
pub struct AttributeProcessor {
    /// Function attributes: function_name -> attribute_name -> value
    function_attrs: HashMap<String, HashMap<String, AttributeValue>>,
    /// Type attributes: type_name -> attribute_name -> value
    type_attrs: HashMap<String, HashMap<String, AttributeValue>>,
}

impl Default for AttributeProcessor {
    fn default() -> Self {
        Self::new()
    }
}

impl AttributeProcessor {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            function_attrs: HashMap::new(),
            type_attrs: HashMap::new(),
        }
    }

    /// Register a function attribute
    #[allow(dead_code)]
    pub fn register_function_attr(
        &mut self,
        func_name: String,
        attr_name: String,
        value: AttributeValue,
    ) {
        self.function_attrs
            .entry(func_name)
            .or_default()
            .insert(attr_name, value);
    }

    /// Register a type attribute
    pub fn register_type_attr(
        &mut self,
        type_name: String,
        attr_name: String,
        value: AttributeValue,
    ) {
        self.type_attrs
            .entry(type_name)
            .or_default()
            .insert(attr_name, value);
    }

    /// Check if a function has a specific attribute
    pub fn has_function_attr(&self, func_name: &str, attr_name: &str) -> bool {
        self.function_attrs
            .get(func_name)
            .and_then(|attrs| attrs.get(attr_name))
            .is_some()
    }

    /// Check if a type has a specific attribute
    pub fn has_type_attr(&self, type_name: &str, attr_name: &str) -> bool {
        self.type_attrs
            .get(type_name)
            .and_then(|attrs| attrs.get(attr_name))
            .is_some()
    }

    /// Get function attribute value
    pub fn get_function_attr(&self, func_name: &str, attr_name: &str) -> Option<&AttributeValue> {
        self.function_attrs
            .get(func_name)
            .and_then(|attrs| attrs.get(attr_name))
    }

    /// Get type attribute value
    pub fn get_type_attr(&self, type_name: &str, attr_name: &str) -> Option<&AttributeValue> {
        self.type_attrs
            .get(type_name)
            .and_then(|attrs| attrs.get(attr_name))
    }

    /// Get all derive traits for a type
    pub fn get_derive_traits(&self, type_name: &str) -> Vec<String> {
        if let Some(AttributeValue::Args(traits)) = self.get_type_attr(type_name, "derive") {
            traits.clone()
        } else {
            Vec::new()
        }
    }

    /// Validate attribute usage
    pub fn validate_attribute(&self, attr_name: &str, args: &[String]) -> CompilerResult<()> {
        match attr_name {
            "inline" | "no_mangle" | "test" => {
                if !args.is_empty() {
                    return Err(CompilerError::internal_error(format!(
                        "Attribute '{attr_name}' does not take arguments"
                    )));
                }
            }
            "derive" => {
                if args.is_empty() {
                    return Err(CompilerError::internal_error(
                        "Attribute 'derive' requires at least one trait".to_string(),
                    ));
                }
                // Validate known derive traits
                for trait_name in args {
                    if !matches!(
                        trait_name.as_str(),
                        "Clone" | "Debug" | "PartialEq" | "Eq" | "PartialOrd" | "Ord" | "Hash"
                    ) {
                        return Err(CompilerError::internal_error(format!(
                            "Unknown derive trait: {trait_name}"
                        )));
                    }
                }
            }
            _ => {
                return Err(CompilerError::internal_error(format!(
                    "Unknown attribute: {attr_name}"
                )));
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_attribute_processor_creation() {
        let processor = AttributeProcessor::new();
        assert!(!processor.has_function_attr("test", "inline"));
    }

    #[test]
    fn test_register_function_attr() {
        let mut processor = AttributeProcessor::new();
        processor.register_function_attr(
            "my_func".to_string(),
            "inline".to_string(),
            AttributeValue::Flag,
        );
        assert!(processor.has_function_attr("my_func", "inline"));
    }

    #[test]
    fn test_register_type_attr() {
        let mut processor = AttributeProcessor::new();
        processor.register_type_attr(
            "MyStruct".to_string(),
            "derive".to_string(),
            AttributeValue::Args(vec!["Clone".to_string(), "Debug".to_string()]),
        );
        assert!(processor.has_type_attr("MyStruct", "derive"));
        let traits = processor.get_derive_traits("MyStruct");
        assert_eq!(traits, vec!["Clone", "Debug"]);
    }

    #[test]
    fn test_validate_inline_attribute() {
        let processor = AttributeProcessor::new();
        assert!(processor.validate_attribute("inline", &[]).is_ok());
        assert!(processor
            .validate_attribute("inline", &["arg".to_string()])
            .is_err());
    }

    #[test]
    fn test_validate_derive_attribute() {
        let processor = AttributeProcessor::new();
        assert!(processor
            .validate_attribute("derive", &["Clone".to_string()])
            .is_ok());
        assert!(processor.validate_attribute("derive", &[]).is_err());
        assert!(processor
            .validate_attribute("derive", &["UnknownTrait".to_string()])
            .is_err());
    }
}
