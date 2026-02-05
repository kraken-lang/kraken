//! Diagnostic code registry for documentation and lookup.
//!
//! This module provides a registry of all diagnostic codes with their
//! descriptions, examples, and suggestions for fixing errors.

#![allow(dead_code)]

use crate::diagnostics::DiagnosticCode;
use std::collections::HashMap;

/// Detailed information about a diagnostic code.
#[derive(Debug, Clone)]
pub struct DiagnosticInfo {
    pub code: DiagnosticCode,
    pub title: &'static str,
    pub description: &'static str,
    pub example: Option<&'static str>,
    pub suggestion: Option<&'static str>,
}

/// Registry of all diagnostic codes with their documentation.
pub struct DiagnosticRegistry {
    entries: HashMap<&'static str, DiagnosticInfo>,
}

impl DiagnosticRegistry {
    /// Create a new diagnostic registry with all codes.
    pub fn new() -> Self {
        let mut registry = Self {
            entries: HashMap::new(),
        };
        registry.register_all();
        registry
    }

    /// Register a diagnostic code with its information.
    fn register(&mut self, info: DiagnosticInfo) {
        self.entries.insert(info.code.code(), info);
    }

    /// Look up a diagnostic code by its string representation.
    pub fn lookup(&self, code: &str) -> Option<&DiagnosticInfo> {
        self.entries.get(code)
    }

    /// Get all registered diagnostic codes.
    pub fn all_codes(&self) -> Vec<&DiagnosticInfo> {
        self.entries.values().collect()
    }

    /// Register all diagnostic codes.
    fn register_all(&mut self) {
        // Lexer errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0001_UnexpectedCharacter,
            title: "Unexpected Character",
            description:
                "The lexer encountered a character that is not valid in Kraken source code.",
            example: Some("let x = @;  // '@' is not a valid character"),
            suggestion: Some("Remove or replace the unexpected character with valid syntax."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0002_UnterminatedStringLiteral,
            title: "Unterminated String Literal",
            description: "A string literal was started but never closed with a closing quote.",
            example: Some("let s = \"hello;  // Missing closing quote"),
            suggestion: Some("Add a closing quote to terminate the string literal."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA0004_InvalidNumberFormat,
            title: "Invalid Number Format",
            description: "The number literal has an invalid format that cannot be parsed.",
            example: Some("let x = 123abc;  // Invalid number format"),
            suggestion: Some("Ensure the number follows valid integer or floating-point syntax."),
        });

        // Parser errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1000_UnexpectedToken,
            title: "Unexpected Token",
            description: "The parser encountered a token that was not expected in this context.",
            example: Some("fn foo() { let x = ; }  // Unexpected semicolon"),
            suggestion: Some("Check the syntax and ensure all expressions are complete."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1001_ExpectedToken,
            title: "Expected Token",
            description: "The parser expected a specific token but found something else.",
            example: Some("fn foo() { let x = 5  // Missing semicolon"),
            suggestion: Some("Add the expected token to complete the syntax."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA1010_MissingClosingBrace,
            title: "Missing Closing Brace",
            description: "A block was opened with '{' but never closed with '}'.",
            example: Some("fn foo() { let x = 5;  // Missing closing brace"),
            suggestion: Some("Add a closing brace '}' to complete the block."),
        });

        // Type errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2000_TypeMismatch,
            title: "Type Mismatch",
            description: "The type of an expression does not match the expected type.",
            example: Some("let x: int = \"hello\";  // Expected int, found string"),
            suggestion: Some(
                "Ensure the expression has the correct type or add a type conversion.",
            ),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2001_UndefinedType,
            title: "Undefined Type",
            description: "A type name was used but has not been defined.",
            example: Some("let x: MyType = 5;  // MyType is not defined"),
            suggestion: Some("Define the type or check for typos in the type name."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA2005_ArityMismatch,
            title: "Arity Mismatch",
            description: "A function was called with the wrong number of arguments.",
            example: Some("fn foo(x: int) { }\nfoo(1, 2);  // Expected 1 argument, found 2"),
            suggestion: Some(
                "Provide the correct number of arguments to match the function signature.",
            ),
        });

        // Resolution errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3000_UndefinedVariable,
            title: "Undefined Variable",
            description: "A variable was used but has not been defined in the current scope.",
            example: Some("let y = x + 1;  // x is not defined"),
            suggestion: Some("Define the variable before using it or check for typos."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3001_UndefinedFunction,
            title: "Undefined Function",
            description: "A function was called but has not been defined.",
            example: Some("let result = foo();  // foo is not defined"),
            suggestion: Some("Define the function or import it from a module."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA3008_DuplicateDefinition,
            title: "Duplicate Definition",
            description: "A name was defined more than once in the same scope.",
            example: Some("let x = 5;\nlet x = 10;  // x is already defined"),
            suggestion: Some("Use a different name or remove the duplicate definition."),
        });

        // Borrow checking errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA4000_UseAfterMove,
            title: "Use After Move",
            description: "A value was used after it was moved to another location.",
            example: Some("let x = vec![1, 2, 3];\nlet y = x;\nprintln(x);  // x was moved"),
            suggestion: Some("Clone the value before moving it or use a reference instead."),
        });

        // Codegen errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA5000_CodegenFailure,
            title: "Code Generation Failure",
            description: "The code generator failed to produce output for this code.",
            example: None,
            suggestion: Some("This is likely a compiler bug. Please report it."),
        });

        // I/O errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA9000_FileNotFound,
            title: "File Not Found",
            description: "The specified file could not be found.",
            example: Some("kraken compile missing.kr"),
            suggestion: Some("Check that the file path is correct and the file exists."),
        });

        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA9001_InvalidFileExtension,
            title: "Invalid File Extension",
            description: "The file has an invalid extension. Kraken files must use .kr or .krak.",
            example: Some("kraken compile file.txt"),
            suggestion: Some("Rename the file to use .kr or .krak extension."),
        });

        // Internal errors
        self.register(DiagnosticInfo {
            code: DiagnosticCode::KRA9999_InternalCompilerError,
            title: "Internal Compiler Error",
            description: "An unexpected error occurred inside the compiler.",
            example: None,
            suggestion: Some(
                "This is a compiler bug. Please report it with a minimal reproduction.",
            ),
        });
    }
}

impl Default for DiagnosticRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_creation() {
        let registry = DiagnosticRegistry::new();
        assert!(!registry.entries.is_empty());
    }

    #[test]
    fn test_lookup_existing_code() {
        let registry = DiagnosticRegistry::new();
        let info = registry.lookup("KRA0001");
        assert!(info.is_some());
        assert_eq!(info.unwrap().title, "Unexpected Character");
    }

    #[test]
    fn test_lookup_nonexistent_code() {
        let registry = DiagnosticRegistry::new();
        let info = registry.lookup("KRA9998");
        assert!(info.is_none());
    }

    #[test]
    fn test_all_codes() {
        let registry = DiagnosticRegistry::new();
        let codes = registry.all_codes();
        assert!(!codes.is_empty());
    }

    #[test]
    fn test_diagnostic_info_fields() {
        let registry = DiagnosticRegistry::new();
        let info = registry.lookup("KRA2000").unwrap();
        assert_eq!(info.code, DiagnosticCode::KRA2000_TypeMismatch);
        assert!(!info.title.is_empty());
        assert!(!info.description.is_empty());
    }
}
