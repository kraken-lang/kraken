use std::path::PathBuf;
use thiserror::Error;

/// Compiler result type.
pub type CompilerResult<T> = Result<T, CompilerError>;

/// Error codes for categorization and documentation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum ErrorCode {
    E0001, // Unexpected character in lexer
    E0002, // Unterminated string literal
    E0003, // Invalid number format
    E0004, // Unexpected token in parser
    E0005, // Expected token not found
    E0006, // Invalid syntax
    E0007, // Type mismatch
    E0008, // Undefined variable
    E0009, // Undefined function
    E0010, // Undefined type
    E0011, // Arity mismatch
    E0012, // Invalid operation
    E0013, // Codegen failure
    E0014, // File not found
    E0015, // Invalid file extension
    E0016, // I/O error
    E0017, // Internal compiler error
    E0018, // Multiple errors
}

impl std::fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

/// Comprehensive compiler error types.
#[derive(Error, Debug)]
#[allow(dead_code)]
pub enum CompilerError {
    /// Lexer errors
    #[error("[{code}] Lexer error at {location}: {message}")]
    LexerError {
        code: ErrorCode,
        location: SourceLocation,
        message: String,
    },

    /// Parser errors
    #[error("[{code}] Parser error at {location}: {message}")]
    ParserError {
        code: ErrorCode,
        location: SourceLocation,
        message: String,
    },

    /// Type checking errors
    #[error("[{code}] Type error at {location}: {message}")]
    TypeError {
        code: ErrorCode,
        location: SourceLocation,
        message: String,
    },

    /// Code generation errors
    #[error("[{code}] Code generation error: {message}")]
    CodegenError { code: ErrorCode, message: String },

    /// File I/O errors
    #[error("[E0016] I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// File not found
    #[error("[E0014] File not found: {0}")]
    FileNotFound(PathBuf),

    /// Invalid file extension
    #[error("[E0015] Invalid file extension: expected .kr or .krak, found {0}")]
    InvalidExtension(String),

    /// Multiple errors
    #[error("[E0018] Multiple compilation errors occurred")]
    MultipleErrors(Vec<CompilerError>),

    /// Internal compiler error
    #[error("[E0017] Internal compiler error: {0}")]
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
    #[allow(dead_code)]
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

/// Source span representing a range in the source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceSpan {
    pub file: PathBuf,
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

#[allow(dead_code)]
impl SourceSpan {
    /// Create a new source span.
    pub fn new(
        file: PathBuf,
        start_line: usize,
        start_col: usize,
        end_line: usize,
        end_col: usize,
    ) -> Self {
        Self {
            file,
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }

    /// Create a span from a single location (zero-width).
    pub fn from_location(loc: &SourceLocation) -> Self {
        Self {
            file: loc.file.clone(),
            start_line: loc.line,
            start_col: loc.column,
            end_line: loc.line,
            end_col: loc.column,
        }
    }

    /// Convert to a SourceLocation (uses start position).
    pub fn to_location(&self) -> SourceLocation {
        SourceLocation::new(self.file.clone(), self.start_line, self.start_col)
    }
}

impl std::fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start_line == self.end_line {
            write!(
                f,
                "{}:{}:{}-{}",
                self.file.display(),
                self.start_line,
                self.start_col,
                self.end_col
            )
        } else {
            write!(
                f,
                "{}:{}:{}-{}:{}",
                self.file.display(),
                self.start_line,
                self.start_col,
                self.end_line,
                self.end_col
            )
        }
    }
}

/// Diagnostic hint for helping users fix errors.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct DiagnosticHint {
    pub message: String,
    pub suggestion: Option<String>,
}

#[allow(dead_code)]
impl DiagnosticHint {
    /// Create a new hint with just a message.
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            suggestion: None,
        }
    }

    /// Create a hint with a suggestion.
    pub fn with_suggestion(message: impl Into<String>, suggestion: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            suggestion: Some(suggestion.into()),
        }
    }
}

impl std::fmt::Display for DiagnosticHint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1b[36mhint:\x1b[0m {}", self.message)?;
        if let Some(ref suggestion) = self.suggestion {
            write!(f, "\n      \x1b[32msuggestion:\x1b[0m {suggestion}")?;
        }
        Ok(())
    }
}

/// Enhanced diagnostic with span and hints.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Diagnostic {
    pub span: SourceSpan,
    pub message: String,
    pub hints: Vec<DiagnosticHint>,
    pub severity: DiagnosticSeverity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    #[allow(dead_code)]
    Info,
}

#[allow(dead_code)]
impl Diagnostic {
    /// Create a new error diagnostic.
    pub fn error(span: SourceSpan, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            hints: Vec::new(),
            severity: DiagnosticSeverity::Error,
        }
    }

    /// Create a new warning diagnostic.
    #[allow(dead_code)]
    pub fn warning(span: SourceSpan, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            hints: Vec::new(),
            severity: DiagnosticSeverity::Warning,
        }
    }

    /// Add a hint to the diagnostic.
    pub fn with_hint(mut self, hint: DiagnosticHint) -> Self {
        self.hints.push(hint);
        self
    }

    /// Format the diagnostic with source context.
    pub fn format_with_source(&self, source: &str) -> String {
        let severity_str = match self.severity {
            DiagnosticSeverity::Error => "\x1b[31merror\x1b[0m",
            DiagnosticSeverity::Warning => "\x1b[33mwarning\x1b[0m",
            DiagnosticSeverity::Info => "\x1b[36minfo\x1b[0m",
        };

        let mut output = format!(
            "{}: {}\n  \x1b[34m-->\x1b[0m {}\n",
            severity_str, self.message, self.span
        );

        // Show source context
        let lines: Vec<&str> = source.lines().collect();
        if self.span.start_line > 0 && self.span.start_line <= lines.len() {
            use std::fmt::Write as _;

            let line_num = self.span.start_line;
            let line = lines[line_num - 1];
            let line_num_width = format!("{line_num}").len();

            output.push_str("   \x1b[34m|\x1b[0m\n");
            let _ = writeln!(
                &mut output,
                "\x1b[34m{line_num:>line_num_width$} |\x1b[0m {line}"
            );

            // Underline the error span
            let start = self.span.start_col.saturating_sub(1);
            let end = if self.span.start_line == self.span.end_line {
                self.span.end_col.saturating_sub(1).max(start + 1)
            } else {
                line.len()
            };
            let underline_len = end.saturating_sub(start).max(1);

            output.push_str(&format!(
                "   \x1b[34m|\x1b[0m {}\x1b[31m{}\x1b[0m\n",
                " ".repeat(start),
                "^".repeat(underline_len)
            ));
        }

        // Add hints
        for hint in &self.hints {
            output.push_str(&format!("   {hint}\n"));
        }

        output
    }
}

impl CompilerError {
    /// Create a lexer error.
    pub fn lexer_error(location: SourceLocation, message: impl Into<String>) -> Self {
        Self::LexerError {
            code: ErrorCode::E0001,
            location,
            message: message.into(),
        }
    }

    /// Create a parser error.
    pub fn parser_error(location: SourceLocation, message: impl Into<String>) -> Self {
        Self::ParserError {
            code: ErrorCode::E0004,
            location,
            message: message.into(),
        }
    }

    /// Create a type error.
    pub fn type_error(location: SourceLocation, message: impl Into<String>) -> Self {
        Self::TypeError {
            code: ErrorCode::E0007,
            location,
            message: message.into(),
        }
    }

    /// Create a codegen error.
    pub fn codegen_error(message: impl Into<String>) -> Self {
        Self::CodegenError {
            code: ErrorCode::E0013,
            message: message.into(),
        }
    }

    /// Create an internal error.
    #[allow(dead_code)]
    pub fn internal_error(message: impl Into<String>) -> Self {
        Self::InternalError(message.into())
    }

    /// Get the error code for this error.
    #[allow(dead_code)]
    pub fn code(&self) -> ErrorCode {
        match self {
            Self::LexerError { code, .. } => *code,
            Self::ParserError { code, .. } => *code,
            Self::TypeError { code, .. } => *code,
            Self::CodegenError { code, .. } => *code,
            Self::IoError(_) => ErrorCode::E0016,
            Self::FileNotFound(_) => ErrorCode::E0014,
            Self::InvalidExtension(_) => ErrorCode::E0015,
            Self::MultipleErrors(_) => ErrorCode::E0018,
            Self::InternalError(_) => ErrorCode::E0017,
        }
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

    #[test]
    fn test_source_span_display() {
        let span = SourceSpan::new(PathBuf::from("test.kr"), 5, 10, 5, 20);
        assert_eq!(span.to_string(), "test.kr:5:10-20");

        let multiline = SourceSpan::new(PathBuf::from("test.kr"), 5, 10, 7, 5);
        assert_eq!(multiline.to_string(), "test.kr:5:10-7:5");
    }

    #[test]
    fn test_diagnostic_hint_display() {
        let hint = DiagnosticHint::new("variable not in scope");
        assert!(hint.to_string().contains("variable not in scope"));

        let hint_with_suggestion =
            DiagnosticHint::with_suggestion("did you mean", "use `let` to declare a variable");
        let s = hint_with_suggestion.to_string();
        assert!(s.contains("did you mean"));
        assert!(s.contains("use `let`"));
    }

    #[test]
    fn test_diagnostic_format_with_source() {
        let span = SourceSpan::new(PathBuf::from("test.kr"), 2, 5, 2, 10);
        let diag = Diagnostic::error(span, "undefined variable `foo`").with_hint(
            DiagnosticHint::with_suggestion("did you mean `bar`?", "bar"),
        );

        let source = "fn main() {\n    foo + 1\n}";
        let output = diag.format_with_source(source);

        assert!(output.contains("error"));
        assert!(output.contains("undefined variable"));
        assert!(output.contains("foo + 1"));
        assert!(output.contains("^^^"));
        assert!(output.contains("did you mean"));
    }
}
