//! Full error reporting system for bootstrap compiler.
//!
//! Provides comprehensive error diagnostics with source context and suggestions.

use std::fmt;

/// Error severity levels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Note,
    Warning,
    Error,
    Fatal,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Note => write!(f, "note"),
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
            Severity::Fatal => write!(f, "fatal"),
        }
    }
}

/// Source location for error reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    /// Create a new source location.
    pub fn new(file: String, line: usize, column: usize) -> Self {
        Self { file, line, column }
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

/// Source span for error reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceSpan {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl SourceSpan {
    /// Create a new source span.
    pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
        Self { start, end }
    }

    /// Create a single-point span.
    pub fn point(location: SourceLocation) -> Self {
        Self {
            start: location.clone(),
            end: location,
        }
    }
}

/// Diagnostic message with source context.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Option<SourceSpan>,
    pub notes: Vec<String>,
    pub suggestions: Vec<String>,
    pub code: Option<String>,
}

impl Diagnostic {
    /// Create a new diagnostic.
    pub fn new(severity: Severity, message: String) -> Self {
        Self {
            severity,
            message,
            span: None,
            notes: Vec::new(),
            suggestions: Vec::new(),
            code: None,
        }
    }

    /// Add a source span.
    pub fn with_span(mut self, span: SourceSpan) -> Self {
        self.span = Some(span);
        self
    }

    /// Add a note.
    pub fn with_note(mut self, note: String) -> Self {
        self.notes.push(note);
        self
    }

    /// Add a suggestion.
    pub fn with_suggestion(mut self, suggestion: String) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    /// Add an error code.
    pub fn with_code(mut self, code: String) -> Self {
        self.code = Some(code);
        self
    }

    /// Format the diagnostic for display.
    pub fn format(&self, source: Option<&str>) -> String {
        let mut output = String::new();

        // Header line
        if let Some(code) = &self.code {
            output.push_str(&format!("{} [{}]: {}\n", self.severity, code, self.message));
        } else {
            output.push_str(&format!("{}: {}\n", self.severity, self.message));
        }

        // Location
        if let Some(span) = &self.span {
            output.push_str(&format!("  --> {}\n", span.start));

            // Source context
            if let Some(src) = source {
                let lines: Vec<&str> = src.lines().collect();
                if span.start.line > 0 && span.start.line <= lines.len() {
                    let line = lines[span.start.line - 1];
                    output.push_str("   |\n");
                    output.push_str(&format!("{:3} | {}\n", span.start.line, line));
                    output.push_str(&format!(
                        "   | {}\n",
                        " ".repeat(span.start.column - 1) + "^"
                    ));
                }
            }
        }

        // Notes
        for note in &self.notes {
            output.push_str(&format!("  = note: {note}\n"));
        }

        // Suggestions
        for suggestion in &self.suggestions {
            output.push_str(&format!("  = help: {suggestion}\n"));
        }

        output
    }
}

/// Error reporter for collecting and displaying diagnostics.
#[derive(Debug, Clone)]
pub struct ErrorReporter {
    diagnostics: Vec<Diagnostic>,
    error_count: usize,
    warning_count: usize,
}

impl ErrorReporter {
    /// Create a new error reporter.
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            error_count: 0,
            warning_count: 0,
        }
    }

    /// Report a diagnostic.
    pub fn report(&mut self, diagnostic: Diagnostic) {
        match diagnostic.severity {
            Severity::Error | Severity::Fatal => self.error_count += 1,
            Severity::Warning => self.warning_count += 1,
            Severity::Note => {}
        }
        self.diagnostics.push(diagnostic);
    }

    /// Report an error.
    pub fn error(&mut self, message: String) {
        self.report(Diagnostic::new(Severity::Error, message));
    }

    /// Report a warning.
    pub fn warning(&mut self, message: String) {
        self.report(Diagnostic::new(Severity::Warning, message));
    }

    /// Report a note.
    pub fn note(&mut self, message: String) {
        self.report(Diagnostic::new(Severity::Note, message));
    }

    /// Get all diagnostics.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    /// Get error count.
    pub fn error_count(&self) -> usize {
        self.error_count
    }

    /// Get warning count.
    pub fn warning_count(&self) -> usize {
        self.warning_count
    }

    /// Clear all diagnostics.
    pub fn clear(&mut self) {
        self.diagnostics.clear();
        self.error_count = 0;
        self.warning_count = 0;
    }

    /// Format all diagnostics.
    pub fn format_all(&self, source: Option<&str>) -> String {
        let mut output = String::new();
        for diagnostic in &self.diagnostics {
            output.push_str(&diagnostic.format(source));
            output.push('\n');
        }

        if self.error_count > 0 || self.warning_count > 0 {
            output.push_str(&format!(
                "compilation finished with {} error(s) and {} warning(s)\n",
                self.error_count, self.warning_count
            ));
        }

        output
    }
}

impl Default for ErrorReporter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_severity_ordering() {
        assert!(Severity::Note < Severity::Warning);
        assert!(Severity::Warning < Severity::Error);
        assert!(Severity::Error < Severity::Fatal);
    }

    #[test]
    fn test_source_location() {
        let loc = SourceLocation::new("test.kr".to_string(), 10, 5);
        assert_eq!(loc.file, "test.kr");
        assert_eq!(loc.line, 10);
        assert_eq!(loc.column, 5);
        assert_eq!(loc.to_string(), "test.kr:10:5");
    }

    #[test]
    fn test_source_span() {
        let start = SourceLocation::new("test.kr".to_string(), 1, 1);
        let end = SourceLocation::new("test.kr".to_string(), 1, 10);
        let span = SourceSpan::new(start, end);
        assert_eq!(span.start.line, 1);
        assert_eq!(span.end.column, 10);
    }

    #[test]
    fn test_diagnostic_builder() {
        let diag = Diagnostic::new(Severity::Error, "test error".to_string())
            .with_code("E001".to_string())
            .with_note("this is a note".to_string())
            .with_suggestion("try this instead".to_string());

        assert_eq!(diag.severity, Severity::Error);
        assert_eq!(diag.message, "test error");
        assert_eq!(diag.code, Some("E001".to_string()));
        assert_eq!(diag.notes.len(), 1);
        assert_eq!(diag.suggestions.len(), 1);
    }

    #[test]
    fn test_error_reporter() {
        let mut reporter = ErrorReporter::new();
        assert!(!reporter.has_errors());
        assert_eq!(reporter.error_count(), 0);

        reporter.error("test error".to_string());
        assert!(reporter.has_errors());
        assert_eq!(reporter.error_count(), 1);

        reporter.warning("test warning".to_string());
        assert_eq!(reporter.warning_count(), 1);
    }

    #[test]
    fn test_error_reporter_clear() {
        let mut reporter = ErrorReporter::new();
        reporter.error("error 1".to_string());
        reporter.warning("warning 1".to_string());
        assert_eq!(reporter.error_count(), 1);
        assert_eq!(reporter.warning_count(), 1);

        reporter.clear();
        assert_eq!(reporter.error_count(), 0);
        assert_eq!(reporter.warning_count(), 0);
    }

    #[test]
    fn test_diagnostic_format() {
        let loc = SourceLocation::new("test.kr".to_string(), 1, 5);
        let span = SourceSpan::point(loc);
        let diag = Diagnostic::new(Severity::Error, "undefined variable".to_string())
            .with_span(span)
            .with_code("E001".to_string())
            .with_suggestion("did you mean 'count'?".to_string());

        let formatted = diag.format(Some("let x = count + 1;"));
        assert!(formatted.contains("error [E001]"));
        assert!(formatted.contains("undefined variable"));
        assert!(formatted.contains("help"));
    }

    #[test]
    fn test_error_reporter_format_all() {
        let mut reporter = ErrorReporter::new();
        reporter.error("error 1".to_string());
        reporter.warning("warning 1".to_string());

        let output = reporter.format_all(None);
        assert!(output.contains("error: error 1"));
        assert!(output.contains("warning: warning 1"));
        assert!(output.contains("1 error(s)"));
        assert!(output.contains("1 warning(s)"));
    }
}
