//! Comprehensive diagnostic code system for the Kraken compiler.
//!
//! This module provides a structured diagnostic system with unique error codes,
//! severity levels, categories, and detailed error messages with suggestions.

#![allow(dead_code)]

use std::fmt;

/// Diagnostic severity levels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticSeverity {
    /// Informational note
    Note,
    /// Helpful suggestion
    Help,
    /// Warning that doesn't prevent compilation
    Warning,
    /// Error that prevents compilation
    Error,
}

impl fmt::Display for DiagnosticSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticSeverity::Note => write!(f, "note"),
            DiagnosticSeverity::Help => write!(f, "help"),
            DiagnosticSeverity::Warning => write!(f, "warning"),
            DiagnosticSeverity::Error => write!(f, "error"),
        }
    }
}

/// Diagnostic code categories for organization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticCategory {
    /// Lexical analysis errors (KRA0001-KRA0999)
    Lexer,
    /// Parser errors (KRA1000-KRA1999)
    Parser,
    /// Type system errors (KRA2000-KRA2999)
    Type,
    /// Name resolution errors (KRA3000-KRA3999)
    Resolution,
    /// Borrow checking errors (KRA4000-KRA4999)
    Borrow,
    /// Code generation errors (KRA5000-KRA5999)
    Codegen,
    /// Module system errors (KRA6000-KRA6999)
    Module,
    /// Macro expansion errors (KRA7000-KRA7999)
    Macro,
    /// Attribute errors (KRA8000-KRA8999)
    Attribute,
    /// I/O and file system errors (KRA9000-KRA9999)
    Io,
}

/// Comprehensive diagnostic codes for the Kraken compiler.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum DiagnosticCode {
    // Lexer errors (KRA0001-KRA0999)
    KRA0001_UnexpectedCharacter,
    KRA0002_UnterminatedStringLiteral,
    KRA0003_UnterminatedCharLiteral,
    KRA0004_InvalidNumberFormat,
    KRA0005_InvalidEscapeSequence,
    KRA0006_InvalidUnicodeEscape,
    KRA0007_UnterminatedBlockComment,
    KRA0008_InvalidFloatLiteral,
    KRA0009_IntegerLiteralTooLarge,
    KRA0010_InvalidBinaryLiteral,
    KRA0011_InvalidOctalLiteral,
    KRA0012_InvalidHexLiteral,

    // Parser errors (KRA1000-KRA1999)
    KRA1000_UnexpectedToken,
    KRA1001_ExpectedToken,
    KRA1002_UnexpectedEof,
    KRA1003_InvalidSyntax,
    KRA1004_MissingExpression,
    KRA1005_MissingStatement,
    KRA1006_InvalidPattern,
    KRA1007_InvalidTypeAnnotation,
    KRA1008_MissingFunctionBody,
    KRA1009_InvalidFunctionSignature,
    KRA1010_MissingClosingBrace,
    KRA1011_MissingClosingParen,
    KRA1012_MissingClosingBracket,
    KRA1013_InvalidAttribute,
    KRA1014_DuplicateParameter,
    KRA1015_InvalidVisibility,

    // Type errors (KRA2000-KRA2999)
    KRA2000_TypeMismatch,
    KRA2001_UndefinedType,
    KRA2002_TypeNotInferred,
    KRA2003_RecursiveType,
    KRA2004_InvalidCast,
    KRA2005_ArityMismatch,
    KRA2006_MissingTypeParameter,
    KRA2007_TooManyTypeParameters,
    KRA2008_TypeParameterBoundNotSatisfied,
    KRA2009_TraitNotImplemented,
    KRA2010_AmbiguousType,
    KRA2011_InvalidTypeInContext,
    KRA2012_CannotInferType,
    KRA2013_CyclicTypeAlias,
    KRA2014_InvalidSelfType,
    KRA2015_MismatchedReturnType,

    // Resolution errors (KRA3000-KRA3999)
    KRA3000_UndefinedVariable,
    KRA3001_UndefinedFunction,
    KRA3002_UndefinedModule,
    KRA3003_UndefinedStruct,
    KRA3004_UndefinedEnum,
    KRA3005_UndefinedTrait,
    KRA3006_AmbiguousName,
    KRA3007_PrivateAccess,
    KRA3008_DuplicateDefinition,
    KRA3009_CircularDependency,
    KRA3010_InvalidImport,
    KRA3011_ModuleNotFound,
    KRA3012_CannotResolveSymbol,

    // Borrow checking errors (KRA4000-KRA4999)
    KRA4000_UseAfterMove,
    KRA4001_UseAfterFree,
    KRA4002_DoubleFree,
    KRA4003_BorrowWhileMutable,
    KRA4004_MutableBorrowWhileBorrowed,
    KRA4005_CannotMoveOutOfBorrow,
    KRA4006_LifetimeTooShort,
    KRA4007_CannotReturnReference,
    KRA4008_DanglingReference,

    // Codegen errors (KRA5000-KRA5999)
    KRA5000_CodegenFailure,
    KRA5001_LlvmError,
    KRA5002_LinkError,
    KRA5003_InvalidTarget,
    KRA5004_OptimizationError,
    KRA5005_AssemblyError,

    // Module system errors (KRA6000-KRA6999)
    KRA6000_ModuleNotFound,
    KRA6001_CyclicModuleDependency,
    KRA6002_InvalidModulePath,
    KRA6003_DuplicateModule,
    KRA6004_InvalidModuleStructure,

    // Macro errors (KRA7000-KRA7999)
    KRA7000_MacroExpansionError,
    KRA7001_InvalidMacroInvocation,
    KRA7002_MacroNotFound,
    KRA7003_RecursiveMacroExpansion,

    // Attribute errors (KRA8000-KRA8999)
    KRA8000_UnknownAttribute,
    KRA8001_InvalidAttributeArgument,
    KRA8002_MissingAttributeArgument,
    KRA8003_DuplicateAttribute,

    // I/O errors (KRA9000-KRA9999)
    KRA9000_FileNotFound,
    KRA9001_InvalidFileExtension,
    KRA9002_IoError,
    KRA9003_PermissionDenied,
    KRA9004_InvalidPath,

    // Internal errors (KRA9999)
    KRA9999_InternalCompilerError,
}

impl DiagnosticCode {
    /// Get the numeric code as a string (e.g., "KRA0001").
    pub fn code(&self) -> &'static str {
        match self {
            // Lexer
            DiagnosticCode::KRA0001_UnexpectedCharacter => "KRA0001",
            DiagnosticCode::KRA0002_UnterminatedStringLiteral => "KRA0002",
            DiagnosticCode::KRA0003_UnterminatedCharLiteral => "KRA0003",
            DiagnosticCode::KRA0004_InvalidNumberFormat => "KRA0004",
            DiagnosticCode::KRA0005_InvalidEscapeSequence => "KRA0005",
            DiagnosticCode::KRA0006_InvalidUnicodeEscape => "KRA0006",
            DiagnosticCode::KRA0007_UnterminatedBlockComment => "KRA0007",
            DiagnosticCode::KRA0008_InvalidFloatLiteral => "KRA0008",
            DiagnosticCode::KRA0009_IntegerLiteralTooLarge => "KRA0009",
            DiagnosticCode::KRA0010_InvalidBinaryLiteral => "KRA0010",
            DiagnosticCode::KRA0011_InvalidOctalLiteral => "KRA0011",
            DiagnosticCode::KRA0012_InvalidHexLiteral => "KRA0012",

            // Parser
            DiagnosticCode::KRA1000_UnexpectedToken => "KRA1000",
            DiagnosticCode::KRA1001_ExpectedToken => "KRA1001",
            DiagnosticCode::KRA1002_UnexpectedEof => "KRA1002",
            DiagnosticCode::KRA1003_InvalidSyntax => "KRA1003",
            DiagnosticCode::KRA1004_MissingExpression => "KRA1004",
            DiagnosticCode::KRA1005_MissingStatement => "KRA1005",
            DiagnosticCode::KRA1006_InvalidPattern => "KRA1006",
            DiagnosticCode::KRA1007_InvalidTypeAnnotation => "KRA1007",
            DiagnosticCode::KRA1008_MissingFunctionBody => "KRA1008",
            DiagnosticCode::KRA1009_InvalidFunctionSignature => "KRA1009",
            DiagnosticCode::KRA1010_MissingClosingBrace => "KRA1010",
            DiagnosticCode::KRA1011_MissingClosingParen => "KRA1011",
            DiagnosticCode::KRA1012_MissingClosingBracket => "KRA1012",
            DiagnosticCode::KRA1013_InvalidAttribute => "KRA1013",
            DiagnosticCode::KRA1014_DuplicateParameter => "KRA1014",
            DiagnosticCode::KRA1015_InvalidVisibility => "KRA1015",

            // Type
            DiagnosticCode::KRA2000_TypeMismatch => "KRA2000",
            DiagnosticCode::KRA2001_UndefinedType => "KRA2001",
            DiagnosticCode::KRA2002_TypeNotInferred => "KRA2002",
            DiagnosticCode::KRA2003_RecursiveType => "KRA2003",
            DiagnosticCode::KRA2004_InvalidCast => "KRA2004",
            DiagnosticCode::KRA2005_ArityMismatch => "KRA2005",
            DiagnosticCode::KRA2006_MissingTypeParameter => "KRA2006",
            DiagnosticCode::KRA2007_TooManyTypeParameters => "KRA2007",
            DiagnosticCode::KRA2008_TypeParameterBoundNotSatisfied => "KRA2008",
            DiagnosticCode::KRA2009_TraitNotImplemented => "KRA2009",
            DiagnosticCode::KRA2010_AmbiguousType => "KRA2010",
            DiagnosticCode::KRA2011_InvalidTypeInContext => "KRA2011",
            DiagnosticCode::KRA2012_CannotInferType => "KRA2012",
            DiagnosticCode::KRA2013_CyclicTypeAlias => "KRA2013",
            DiagnosticCode::KRA2014_InvalidSelfType => "KRA2014",
            DiagnosticCode::KRA2015_MismatchedReturnType => "KRA2015",

            // Resolution
            DiagnosticCode::KRA3000_UndefinedVariable => "KRA3000",
            DiagnosticCode::KRA3001_UndefinedFunction => "KRA3001",
            DiagnosticCode::KRA3002_UndefinedModule => "KRA3002",
            DiagnosticCode::KRA3003_UndefinedStruct => "KRA3003",
            DiagnosticCode::KRA3004_UndefinedEnum => "KRA3004",
            DiagnosticCode::KRA3005_UndefinedTrait => "KRA3005",
            DiagnosticCode::KRA3006_AmbiguousName => "KRA3006",
            DiagnosticCode::KRA3007_PrivateAccess => "KRA3007",
            DiagnosticCode::KRA3008_DuplicateDefinition => "KRA3008",
            DiagnosticCode::KRA3009_CircularDependency => "KRA3009",
            DiagnosticCode::KRA3010_InvalidImport => "KRA3010",
            DiagnosticCode::KRA3011_ModuleNotFound => "KRA3011",
            DiagnosticCode::KRA3012_CannotResolveSymbol => "KRA3012",

            // Borrow
            DiagnosticCode::KRA4000_UseAfterMove => "KRA4000",
            DiagnosticCode::KRA4001_UseAfterFree => "KRA4001",
            DiagnosticCode::KRA4002_DoubleFree => "KRA4002",
            DiagnosticCode::KRA4003_BorrowWhileMutable => "KRA4003",
            DiagnosticCode::KRA4004_MutableBorrowWhileBorrowed => "KRA4004",
            DiagnosticCode::KRA4005_CannotMoveOutOfBorrow => "KRA4005",
            DiagnosticCode::KRA4006_LifetimeTooShort => "KRA4006",
            DiagnosticCode::KRA4007_CannotReturnReference => "KRA4007",
            DiagnosticCode::KRA4008_DanglingReference => "KRA4008",

            // Codegen
            DiagnosticCode::KRA5000_CodegenFailure => "KRA5000",
            DiagnosticCode::KRA5001_LlvmError => "KRA5001",
            DiagnosticCode::KRA5002_LinkError => "KRA5002",
            DiagnosticCode::KRA5003_InvalidTarget => "KRA5003",
            DiagnosticCode::KRA5004_OptimizationError => "KRA5004",
            DiagnosticCode::KRA5005_AssemblyError => "KRA5005",

            // Module
            DiagnosticCode::KRA6000_ModuleNotFound => "KRA6000",
            DiagnosticCode::KRA6001_CyclicModuleDependency => "KRA6001",
            DiagnosticCode::KRA6002_InvalidModulePath => "KRA6002",
            DiagnosticCode::KRA6003_DuplicateModule => "KRA6003",
            DiagnosticCode::KRA6004_InvalidModuleStructure => "KRA6004",

            // Macro
            DiagnosticCode::KRA7000_MacroExpansionError => "KRA7000",
            DiagnosticCode::KRA7001_InvalidMacroInvocation => "KRA7001",
            DiagnosticCode::KRA7002_MacroNotFound => "KRA7002",
            DiagnosticCode::KRA7003_RecursiveMacroExpansion => "KRA7003",

            // Attribute
            DiagnosticCode::KRA8000_UnknownAttribute => "KRA8000",
            DiagnosticCode::KRA8001_InvalidAttributeArgument => "KRA8001",
            DiagnosticCode::KRA8002_MissingAttributeArgument => "KRA8002",
            DiagnosticCode::KRA8003_DuplicateAttribute => "KRA8003",

            // I/O
            DiagnosticCode::KRA9000_FileNotFound => "KRA9000",
            DiagnosticCode::KRA9001_InvalidFileExtension => "KRA9001",
            DiagnosticCode::KRA9002_IoError => "KRA9002",
            DiagnosticCode::KRA9003_PermissionDenied => "KRA9003",
            DiagnosticCode::KRA9004_InvalidPath => "KRA9004",

            // Internal
            DiagnosticCode::KRA9999_InternalCompilerError => "KRA9999",
        }
    }

    /// Get the category of this diagnostic code.
    pub fn category(&self) -> DiagnosticCategory {
        let code_num = self.code()[3..].parse::<u32>().unwrap_or(9999);
        match code_num {
            0..=999 => DiagnosticCategory::Lexer,
            1000..=1999 => DiagnosticCategory::Parser,
            2000..=2999 => DiagnosticCategory::Type,
            3000..=3999 => DiagnosticCategory::Resolution,
            4000..=4999 => DiagnosticCategory::Borrow,
            5000..=5999 => DiagnosticCategory::Codegen,
            6000..=6999 => DiagnosticCategory::Module,
            7000..=7999 => DiagnosticCategory::Macro,
            8000..=8999 => DiagnosticCategory::Attribute,
            9000..=9998 => DiagnosticCategory::Io,
            _ => DiagnosticCategory::Codegen,
        }
    }

    /// Get the default severity for this diagnostic code.
    pub fn default_severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }

    /// Get a short description of this diagnostic code.
    pub fn description(&self) -> &'static str {
        match self {
            DiagnosticCode::KRA0001_UnexpectedCharacter => "unexpected character in source",
            DiagnosticCode::KRA0002_UnterminatedStringLiteral => "unterminated string literal",
            DiagnosticCode::KRA0003_UnterminatedCharLiteral => "unterminated character literal",
            DiagnosticCode::KRA0004_InvalidNumberFormat => "invalid number format",
            DiagnosticCode::KRA0005_InvalidEscapeSequence => "invalid escape sequence",
            DiagnosticCode::KRA0006_InvalidUnicodeEscape => "invalid unicode escape",
            DiagnosticCode::KRA0007_UnterminatedBlockComment => "unterminated block comment",
            DiagnosticCode::KRA0008_InvalidFloatLiteral => "invalid floating-point literal",
            DiagnosticCode::KRA0009_IntegerLiteralTooLarge => "integer literal too large",
            DiagnosticCode::KRA0010_InvalidBinaryLiteral => "invalid binary literal",
            DiagnosticCode::KRA0011_InvalidOctalLiteral => "invalid octal literal",
            DiagnosticCode::KRA0012_InvalidHexLiteral => "invalid hexadecimal literal",

            DiagnosticCode::KRA1000_UnexpectedToken => "unexpected token",
            DiagnosticCode::KRA1001_ExpectedToken => "expected token not found",
            DiagnosticCode::KRA1002_UnexpectedEof => "unexpected end of file",
            DiagnosticCode::KRA1003_InvalidSyntax => "invalid syntax",
            DiagnosticCode::KRA1004_MissingExpression => "missing expression",
            DiagnosticCode::KRA1005_MissingStatement => "missing statement",
            DiagnosticCode::KRA1006_InvalidPattern => "invalid pattern",
            DiagnosticCode::KRA1007_InvalidTypeAnnotation => "invalid type annotation",
            DiagnosticCode::KRA1008_MissingFunctionBody => "missing function body",
            DiagnosticCode::KRA1009_InvalidFunctionSignature => "invalid function signature",
            DiagnosticCode::KRA1010_MissingClosingBrace => "missing closing brace",
            DiagnosticCode::KRA1011_MissingClosingParen => "missing closing parenthesis",
            DiagnosticCode::KRA1012_MissingClosingBracket => "missing closing bracket",
            DiagnosticCode::KRA1013_InvalidAttribute => "invalid attribute",
            DiagnosticCode::KRA1014_DuplicateParameter => "duplicate parameter name",
            DiagnosticCode::KRA1015_InvalidVisibility => "invalid visibility modifier",

            DiagnosticCode::KRA2000_TypeMismatch => "type mismatch",
            DiagnosticCode::KRA2001_UndefinedType => "undefined type",
            DiagnosticCode::KRA2002_TypeNotInferred => "type could not be inferred",
            DiagnosticCode::KRA2003_RecursiveType => "recursive type definition",
            DiagnosticCode::KRA2004_InvalidCast => "invalid type cast",
            DiagnosticCode::KRA2005_ArityMismatch => "wrong number of arguments",
            DiagnosticCode::KRA2006_MissingTypeParameter => "missing type parameter",
            DiagnosticCode::KRA2007_TooManyTypeParameters => "too many type parameters",
            DiagnosticCode::KRA2008_TypeParameterBoundNotSatisfied => {
                "type parameter bound not satisfied"
            }
            DiagnosticCode::KRA2009_TraitNotImplemented => "trait not implemented",
            DiagnosticCode::KRA2010_AmbiguousType => "ambiguous type",
            DiagnosticCode::KRA2011_InvalidTypeInContext => "invalid type in this context",
            DiagnosticCode::KRA2012_CannotInferType => "cannot infer type",
            DiagnosticCode::KRA2013_CyclicTypeAlias => "cyclic type alias",
            DiagnosticCode::KRA2014_InvalidSelfType => "invalid self type",
            DiagnosticCode::KRA2015_MismatchedReturnType => "mismatched return type",

            DiagnosticCode::KRA3000_UndefinedVariable => "undefined variable",
            DiagnosticCode::KRA3001_UndefinedFunction => "undefined function",
            DiagnosticCode::KRA3002_UndefinedModule => "undefined module",
            DiagnosticCode::KRA3003_UndefinedStruct => "undefined struct",
            DiagnosticCode::KRA3004_UndefinedEnum => "undefined enum",
            DiagnosticCode::KRA3005_UndefinedTrait => "undefined trait",
            DiagnosticCode::KRA3006_AmbiguousName => "ambiguous name",
            DiagnosticCode::KRA3007_PrivateAccess => "cannot access private item",
            DiagnosticCode::KRA3008_DuplicateDefinition => "duplicate definition",
            DiagnosticCode::KRA3009_CircularDependency => "circular dependency",
            DiagnosticCode::KRA3010_InvalidImport => "invalid import",
            DiagnosticCode::KRA3011_ModuleNotFound => "module not found",
            DiagnosticCode::KRA3012_CannotResolveSymbol => "cannot resolve symbol",

            DiagnosticCode::KRA4000_UseAfterMove => "use of moved value",
            DiagnosticCode::KRA4001_UseAfterFree => "use after free",
            DiagnosticCode::KRA4002_DoubleFree => "double free",
            DiagnosticCode::KRA4003_BorrowWhileMutable => {
                "cannot borrow as immutable while mutable borrow exists"
            }
            DiagnosticCode::KRA4004_MutableBorrowWhileBorrowed => {
                "cannot borrow as mutable while borrowed"
            }
            DiagnosticCode::KRA4005_CannotMoveOutOfBorrow => "cannot move out of borrowed content",
            DiagnosticCode::KRA4006_LifetimeTooShort => "lifetime too short",
            DiagnosticCode::KRA4007_CannotReturnReference => {
                "cannot return reference to local variable"
            }
            DiagnosticCode::KRA4008_DanglingReference => "dangling reference",

            DiagnosticCode::KRA5000_CodegenFailure => "code generation failed",
            DiagnosticCode::KRA5001_LlvmError => "LLVM error",
            DiagnosticCode::KRA5002_LinkError => "linker error",
            DiagnosticCode::KRA5003_InvalidTarget => "invalid target",
            DiagnosticCode::KRA5004_OptimizationError => "optimization error",
            DiagnosticCode::KRA5005_AssemblyError => "assembly error",

            DiagnosticCode::KRA6000_ModuleNotFound => "module not found",
            DiagnosticCode::KRA6001_CyclicModuleDependency => "cyclic module dependency",
            DiagnosticCode::KRA6002_InvalidModulePath => "invalid module path",
            DiagnosticCode::KRA6003_DuplicateModule => "duplicate module",
            DiagnosticCode::KRA6004_InvalidModuleStructure => "invalid module structure",

            DiagnosticCode::KRA7000_MacroExpansionError => "macro expansion error",
            DiagnosticCode::KRA7001_InvalidMacroInvocation => "invalid macro invocation",
            DiagnosticCode::KRA7002_MacroNotFound => "macro not found",
            DiagnosticCode::KRA7003_RecursiveMacroExpansion => "recursive macro expansion",

            DiagnosticCode::KRA8000_UnknownAttribute => "unknown attribute",
            DiagnosticCode::KRA8001_InvalidAttributeArgument => "invalid attribute argument",
            DiagnosticCode::KRA8002_MissingAttributeArgument => "missing attribute argument",
            DiagnosticCode::KRA8003_DuplicateAttribute => "duplicate attribute",

            DiagnosticCode::KRA9000_FileNotFound => "file not found",
            DiagnosticCode::KRA9001_InvalidFileExtension => "invalid file extension",
            DiagnosticCode::KRA9002_IoError => "I/O error",
            DiagnosticCode::KRA9003_PermissionDenied => "permission denied",
            DiagnosticCode::KRA9004_InvalidPath => "invalid path",

            DiagnosticCode::KRA9999_InternalCompilerError => "internal compiler error",
        }
    }
}

impl fmt::Display for DiagnosticCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code())
    }
}

/// Diagnostic message with code, severity, and optional suggestions.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub code: DiagnosticCode,
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub hints: Vec<String>,
    pub suggestions: Vec<String>,
}

impl Diagnostic {
    /// Create a new diagnostic with the given code and message.
    pub fn new(code: DiagnosticCode, message: impl Into<String>) -> Self {
        Self {
            severity: code.default_severity(),
            code,
            message: message.into(),
            hints: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    /// Add a hint to this diagnostic.
    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        self.hints.push(hint.into());
        self
    }

    /// Add a suggestion to this diagnostic.
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestions.push(suggestion.into());
        self
    }

    /// Set the severity of this diagnostic.
    pub fn with_severity(mut self, severity: DiagnosticSeverity) -> Self {
        self.severity = severity;
        self
    }

    /// Format this diagnostic for display.
    pub fn format(&self) -> String {
        let mut output = format!("[{}] {}: {}", self.code, self.severity, self.message);

        for hint in &self.hints {
            output.push_str(&format!("\n  note: {hint}"));
        }

        for suggestion in &self.suggestions {
            output.push_str(&format!("\n  help: {suggestion}"));
        }

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagnostic_code_format() {
        assert_eq!(
            DiagnosticCode::KRA0001_UnexpectedCharacter.code(),
            "KRA0001"
        );
        assert_eq!(DiagnosticCode::KRA2000_TypeMismatch.code(), "KRA2000");
        assert_eq!(
            DiagnosticCode::KRA9999_InternalCompilerError.code(),
            "KRA9999"
        );
    }

    #[test]
    fn test_diagnostic_category() {
        assert_eq!(
            DiagnosticCode::KRA0001_UnexpectedCharacter.category(),
            DiagnosticCategory::Lexer
        );
        assert_eq!(
            DiagnosticCode::KRA1000_UnexpectedToken.category(),
            DiagnosticCategory::Parser
        );
        assert_eq!(
            DiagnosticCode::KRA2000_TypeMismatch.category(),
            DiagnosticCategory::Type
        );
        assert_eq!(
            DiagnosticCode::KRA3000_UndefinedVariable.category(),
            DiagnosticCategory::Resolution
        );
    }

    #[test]
    fn test_diagnostic_description() {
        let desc = DiagnosticCode::KRA0001_UnexpectedCharacter.description();
        assert!(!desc.is_empty());
        assert_eq!(desc, "unexpected character in source");
    }

    #[test]
    fn test_diagnostic_creation() {
        let diag = Diagnostic::new(
            DiagnosticCode::KRA2000_TypeMismatch,
            "expected `int`, found `string`",
        );

        assert_eq!(diag.code, DiagnosticCode::KRA2000_TypeMismatch);
        assert_eq!(diag.severity, DiagnosticSeverity::Error);
        assert_eq!(diag.message, "expected `int`, found `string`");
    }

    #[test]
    fn test_diagnostic_with_hints() {
        let diag = Diagnostic::new(
            DiagnosticCode::KRA3000_UndefinedVariable,
            "cannot find value `x` in this scope",
        )
        .with_hint("variable `x` is not defined")
        .with_suggestion("consider declaring the variable before using it");

        assert_eq!(diag.hints.len(), 1);
        assert_eq!(diag.suggestions.len(), 1);
    }

    #[test]
    fn test_diagnostic_format() {
        let diag = Diagnostic::new(
            DiagnosticCode::KRA2000_TypeMismatch,
            "expected `int`, found `string`",
        )
        .with_hint("types must match")
        .with_suggestion("consider converting the string to an integer");

        let formatted = diag.format();
        assert!(formatted.contains("[KRA2000]"));
        assert!(formatted.contains("error"));
        assert!(formatted.contains("expected `int`, found `string`"));
        assert!(formatted.contains("note: types must match"));
        assert!(formatted.contains("help: consider converting"));
    }

    #[test]
    fn test_severity_ordering() {
        assert!(DiagnosticSeverity::Note < DiagnosticSeverity::Help);
        assert!(DiagnosticSeverity::Help < DiagnosticSeverity::Warning);
        assert!(DiagnosticSeverity::Warning < DiagnosticSeverity::Error);
    }

    #[test]
    fn test_all_codes_have_descriptions() {
        let codes = [
            DiagnosticCode::KRA0001_UnexpectedCharacter,
            DiagnosticCode::KRA1000_UnexpectedToken,
            DiagnosticCode::KRA2000_TypeMismatch,
            DiagnosticCode::KRA3000_UndefinedVariable,
            DiagnosticCode::KRA4000_UseAfterMove,
            DiagnosticCode::KRA5000_CodegenFailure,
            DiagnosticCode::KRA6000_ModuleNotFound,
            DiagnosticCode::KRA7000_MacroExpansionError,
            DiagnosticCode::KRA8000_UnknownAttribute,
            DiagnosticCode::KRA9000_FileNotFound,
            DiagnosticCode::KRA9999_InternalCompilerError,
        ];

        for code in &codes {
            assert!(!code.description().is_empty());
            assert!(!code.code().is_empty());
        }
    }
}
