use std::path::PathBuf;
use thiserror::Error;

/// Compiler result type.
pub type CompilerResult<T> = Result<T, CompilerError>;

/// Comprehensive compiler error types.
#[derive(Error, Debug)]
pub enum CompilerError {
    /// Lexer errors
    #[error("Lexer error at {location}: {message}")]
    LexerError {
        location: SourceLocation,
        message: String,
    },

    /// Parser errors
    #[error("Parser error at {location}: {message}")]
    ParserError {
        location: SourceLocation,
        message: String,
    },

    /// Type checking errors
    #[error("Type error at {location}: {message}")]
    TypeError {
        location: SourceLocation,
        message: String,
    },

    /// Code generation errors
    #[error("Code generation error: {0}")]
    CodegenError(String),

    /// File I/O errors
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// File not found
    #[error("File not found: {0}")]
    FileNotFound(PathBuf),

    /// Invalid file extension
    #[error("Invalid file extension: expected .kr or .krak, found {0}")]
    InvalidExtension(String),

    /// Multiple errors
    #[error("Multiple compilation errors occurred")]
    MultipleErrors(Vec<CompilerError>),

    /// Internal compiler error
    #[error("Internal compiler error: {0}")]
    InternalError(String),
}

/// Source code location for error reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub file: PathBuf,
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    /// Create a new source location.
    pub fn new(file: PathBuf, line: usize, column: usize) -> Self {
        Self { file, line, column }
    }

    /// Create a location at the start of a file.
    pub fn start_of_file(file: PathBuf) -> Self {
        Self {
            file,
            line: 1,
            column: 1,
        }
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file.display(), self.line, self.column)
    }
}

impl CompilerError {
    /// Create a lexer error.
    pub fn lexer_error(location: SourceLocation, message: impl Into<String>) -> Self {
        Self::LexerError {
            location,
            message: message.into(),
        }
    }

    /// Create a parser error.
    pub fn parser_error(location: SourceLocation, message: impl Into<String>) -> Self {
        Self::ParserError {
            location,
            message: message.into(),
        }
    }

    /// Create a type error.
    pub fn type_error(location: SourceLocation, message: impl Into<String>) -> Self {
        Self::TypeError {
            location,
            message: message.into(),
        }
    }

    /// Create a codegen error.
    pub fn codegen_error(message: impl Into<String>) -> Self {
        Self::CodegenError(message.into())
    }

    /// Create an internal error.
    pub fn internal_error(message: impl Into<String>) -> Self {
        Self::InternalError(message.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_location_display() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 10, 5);
        assert_eq!(loc.to_string(), "test.kr:10:5");
    }

    #[test]
    fn test_lexer_error_creation() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 1, 1);
        let err = CompilerError::lexer_error(loc, "unexpected character");
        assert!(matches!(err, CompilerError::LexerError { .. }));
    }

    #[test]
    fn test_error_display() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 5, 10);
        let err = CompilerError::parser_error(loc, "expected semicolon");
        let msg = err.to_string();
        assert!(msg.contains("test.kr:5:10"));
        assert!(msg.contains("expected semicolon"));
    }
}
