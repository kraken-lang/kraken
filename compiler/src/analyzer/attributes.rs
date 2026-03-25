//! Attribute processing and validation for Kraken compiler.

use crate::error::{CompilerError, CompilerResult};
use std::collections::HashMap;

/// Attribute information extracted from AST
#[derive(Debug, Clone, PartialEq)]
pub enum AttributeValue {
    /// Simple flag attribute (e.g., `#[inline]`)
    Flag,
    /// Attribute with arguments (e.g., #[derive(Clone, Debug)])
    Args(Vec<String>),
}

/// Attribute processor for managing function and type attributes
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
    /// Create a new attribute processor with empty registries.
    pub fn new() -> Self {
        Self {
            function_attrs: HashMap::new(),
            type_attrs: HashMap::new(),
        }
    }

    /// Register a function attribute
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
    fn test_default() {
        let p = AttributeProcessor::default();
        assert!(!p.has_function_attr("f", "x"));
    }

    #[test]
    fn test_new() {
        let p = AttributeProcessor::new();
        assert!(p.function_attrs.is_empty());
        assert!(p.type_attrs.is_empty());
    }

    // --- Function attributes ---

    #[test]
    fn test_register_function_attr_flag() {
        let mut p = AttributeProcessor::new();
        p.register_function_attr("f".into(), "inline".into(), AttributeValue::Flag);
        assert!(p.has_function_attr("f", "inline"));
        assert!(!p.has_function_attr("f", "test"));
        assert!(!p.has_function_attr("g", "inline"));
    }

    #[test]
    fn test_get_function_attr() {
        let mut p = AttributeProcessor::new();
        p.register_function_attr("f".into(), "inline".into(), AttributeValue::Flag);
        assert_eq!(p.get_function_attr("f", "inline"), Some(&AttributeValue::Flag));
        assert_eq!(p.get_function_attr("f", "test"), None);
        assert_eq!(p.get_function_attr("g", "inline"), None);
    }

    #[test]
    fn test_register_function_attr_args() {
        let mut p = AttributeProcessor::new();
        p.register_function_attr("f".into(), "cfg".into(), AttributeValue::Args(vec!["test".into()]));
        match p.get_function_attr("f", "cfg") {
            Some(AttributeValue::Args(args)) => assert_eq!(args, &["test"]),
            _ => panic!("Expected Args"),
        }
    }

    // --- Type attributes ---

    #[test]
    fn test_register_type_attr() {
        let mut p = AttributeProcessor::new();
        p.register_type_attr("S".into(), "derive".into(), AttributeValue::Args(vec!["Clone".into()]));
        assert!(p.has_type_attr("S", "derive"));
        assert!(!p.has_type_attr("S", "repr"));
        assert!(!p.has_type_attr("T", "derive"));
    }

    #[test]
    fn test_get_type_attr() {
        let mut p = AttributeProcessor::new();
        p.register_type_attr("S".into(), "inline".into(), AttributeValue::Flag);
        assert_eq!(p.get_type_attr("S", "inline"), Some(&AttributeValue::Flag));
        assert_eq!(p.get_type_attr("S", "missing"), None);
        assert_eq!(p.get_type_attr("T", "inline"), None);
    }

    // --- Derive traits ---

    #[test]
    fn test_get_derive_traits() {
        let mut p = AttributeProcessor::new();
        p.register_type_attr("S".into(), "derive".into(), AttributeValue::Args(vec!["Clone".into(), "Debug".into()]));
        assert_eq!(p.get_derive_traits("S"), vec!["Clone", "Debug"]);
    }

    #[test]
    fn test_get_derive_traits_no_derive() {
        let p = AttributeProcessor::new();
        assert!(p.get_derive_traits("S").is_empty());
    }

    #[test]
    fn test_get_derive_traits_flag_not_args() {
        let mut p = AttributeProcessor::new();
        p.register_type_attr("S".into(), "derive".into(), AttributeValue::Flag);
        assert!(p.get_derive_traits("S").is_empty());
    }

    // --- Validate attributes ---

    #[test]
    fn test_validate_inline_ok() {
        let p = AttributeProcessor::new();
        assert!(p.validate_attribute("inline", &[]).is_ok());
    }

    #[test]
    fn test_validate_inline_with_args_err() {
        let p = AttributeProcessor::new();
        assert!(p.validate_attribute("inline", &["x".into()]).is_err());
    }

    #[test]
    fn test_validate_no_mangle_ok() {
        let p = AttributeProcessor::new();
        assert!(p.validate_attribute("no_mangle", &[]).is_ok());
    }

    #[test]
    fn test_validate_no_mangle_with_args_err() {
        let p = AttributeProcessor::new();
        assert!(p.validate_attribute("no_mangle", &["x".into()]).is_err());
    }

    #[test]
    fn test_validate_test_ok() {
        let p = AttributeProcessor::new();
        assert!(p.validate_attribute("test", &[]).is_ok());
    }

    #[test]
    fn test_validate_test_with_args_err() {
        let p = AttributeProcessor::new();
        assert!(p.validate_attribute("test", &["x".into()]).is_err());
    }

    #[test]
    fn test_validate_derive_ok_all_known() {
        let p = AttributeProcessor::new();
        for t in &["Clone", "Debug", "PartialEq", "Eq", "PartialOrd", "Ord", "Hash"] {
            assert!(p.validate_attribute("derive", &[t.to_string()]).is_ok());
        }
    }

    #[test]
    fn test_validate_derive_empty_err() {
        let p = AttributeProcessor::new();
        assert!(p.validate_attribute("derive", &[]).is_err());
    }

    #[test]
    fn test_validate_derive_unknown_trait_err() {
        let p = AttributeProcessor::new();
        assert!(p.validate_attribute("derive", &["Serialize".into()]).is_err());
    }

    #[test]
    fn test_validate_unknown_attribute_err() {
        let p = AttributeProcessor::new();
        assert!(p.validate_attribute("repr", &[]).is_err());
    }

    // --- Multiple attrs on same item ---

    #[test]
    fn test_multiple_attrs_same_function() {
        let mut p = AttributeProcessor::new();
        p.register_function_attr("f".into(), "inline".into(), AttributeValue::Flag);
        p.register_function_attr("f".into(), "test".into(), AttributeValue::Flag);
        assert!(p.has_function_attr("f", "inline"));
        assert!(p.has_function_attr("f", "test"));
    }
}
