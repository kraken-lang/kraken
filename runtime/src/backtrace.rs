//! Error backtrace capture and formatting for debugging.

#![allow(dead_code)]

use std::fmt;

/// Represents a single frame in a backtrace
#[derive(Debug, Clone)]
pub struct BacktraceFrame {
    pub symbol: Option<String>,
    pub filename: Option<String>,
    pub line: Option<u32>,
    pub column: Option<u32>,
}

impl BacktraceFrame {
    /// Create a new backtrace frame
    pub fn new(
        symbol: Option<String>,
        filename: Option<String>,
        line: Option<u32>,
        column: Option<u32>,
    ) -> Self {
        Self {
            symbol,
            filename,
            line,
            column,
        }
    }

    /// Format the frame for display
    pub fn format(&self) -> String {
        let mut result = String::new();

        if let Some(ref symbol) = self.symbol {
            result.push_str(symbol);
        } else {
            result.push_str("<unknown>");
        }

        if let Some(ref filename) = self.filename {
            result.push_str("\n  at ");
            result.push_str(filename);

            if let Some(line) = self.line {
                result.push(':');
                result.push_str(&line.to_string());

                if let Some(column) = self.column {
                    result.push(':');
                    result.push_str(&column.to_string());
                }
            }
        }

        result
    }
}

impl fmt::Display for BacktraceFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

/// Captured backtrace with stack frames
#[derive(Debug, Clone)]
pub struct Backtrace {
    frames: Vec<BacktraceFrame>,
    enabled: bool,
}

impl Backtrace {
    /// Capture the current backtrace
    pub fn capture() -> Self {
        let enabled = Self::is_enabled();
        let frames = Self::capture_frames();

        Self { frames, enabled }
    }

    /// Create a disabled backtrace (no frames captured)
    pub fn disabled() -> Self {
        Self {
            frames: Vec::new(),
            enabled: false,
        }
    }

    /// Check if backtrace capture is enabled
    pub fn is_enabled() -> bool {
        std::env::var("RUST_BACKTRACE")
            .map(|v| v == "1" || v.to_lowercase() == "full")
            .unwrap_or(false)
    }

    /// Capture stack frames
    fn capture_frames() -> Vec<BacktraceFrame> {
        // Create a simple frame indicating backtrace capture
        // Full backtrace support can be added with the backtrace crate in the future
        vec![BacktraceFrame::new(
            Some("<backtrace capture available via RUST_BACKTRACE>".to_string()),
            None,
            None,
            None,
        )]
    }

    /// Get the frames in this backtrace
    pub fn frames(&self) -> &[BacktraceFrame] {
        &self.frames
    }

    /// Check if this backtrace is empty
    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    /// Get the number of frames
    pub fn len(&self) -> usize {
        self.frames.len()
    }

    /// Format the backtrace for display
    pub fn format(&self) -> String {
        if !self.enabled {
            return "backtrace capture disabled (set RUST_BACKTRACE=1)".to_string();
        }

        if self.frames.is_empty() {
            return "backtrace is empty".to_string();
        }

        let mut result = String::new();
        result.push_str("stack backtrace:\n");

        for (i, frame) in self.frames.iter().enumerate() {
            result.push_str(&format!("{:4}: {}\n", i, frame.format()));
        }

        result
    }

    /// Format the backtrace with a limit on the number of frames
    pub fn format_limited(&self, max_frames: usize) -> String {
        if !self.enabled {
            return "backtrace capture disabled (set RUST_BACKTRACE=1)".to_string();
        }

        if self.frames.is_empty() {
            return "backtrace is empty".to_string();
        }

        let mut result = String::new();
        result.push_str("stack backtrace:\n");

        let frames_to_show = self.frames.len().min(max_frames);
        for (i, frame) in self.frames.iter().take(frames_to_show).enumerate() {
            result.push_str(&format!("{:4}: {}\n", i, frame.format()));
        }

        if self.frames.len() > max_frames {
            result.push_str(&format!(
                "   ... {} more frames\n",
                self.frames.len() - max_frames
            ));
        }

        result
    }
}

impl fmt::Display for Backtrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

impl Default for Backtrace {
    fn default() -> Self {
        Self::capture()
    }
}

/// Error type with backtrace support
#[derive(Debug, Clone)]
pub struct ErrorWithBacktrace {
    message: String,
    backtrace: Backtrace,
}

impl ErrorWithBacktrace {
    /// Create a new error with backtrace
    pub fn new<S: Into<String>>(message: S) -> Self {
        Self {
            message: message.into(),
            backtrace: Backtrace::capture(),
        }
    }

    /// Create a new error without capturing backtrace
    pub fn without_backtrace<S: Into<String>>(message: S) -> Self {
        Self {
            message: message.into(),
            backtrace: Backtrace::disabled(),
        }
    }

    /// Get the error message
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Get the backtrace
    pub fn backtrace(&self) -> &Backtrace {
        &self.backtrace
    }

    /// Format the error with backtrace
    pub fn format(&self) -> String {
        format!("Error: {}\n\n{}", self.message, self.backtrace.format())
    }
}

impl fmt::Display for ErrorWithBacktrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

impl std::error::Error for ErrorWithBacktrace {}

/// Utility functions for backtrace handling
pub struct BacktraceUtils;

impl BacktraceUtils {
    /// Capture a backtrace if enabled
    pub fn capture() -> Backtrace {
        Backtrace::capture()
    }

    /// Check if backtrace capture is enabled
    pub fn is_enabled() -> bool {
        Backtrace::is_enabled()
    }

    /// Enable backtrace capture for the current process
    pub fn enable() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }

    /// Disable backtrace capture for the current process
    pub fn disable() {
        std::env::remove_var("RUST_BACKTRACE");
    }

    /// Format a backtrace with default settings
    pub fn format(backtrace: &Backtrace) -> String {
        backtrace.format()
    }

    /// Format a backtrace with a frame limit
    pub fn format_limited(backtrace: &Backtrace, max_frames: usize) -> String {
        backtrace.format_limited(max_frames)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backtrace_frame_creation() {
        let frame = BacktraceFrame::new(
            Some("test_function".to_string()),
            Some("test.rs".to_string()),
            Some(42),
            Some(10),
        );

        assert_eq!(frame.symbol, Some("test_function".to_string()));
        assert_eq!(frame.filename, Some("test.rs".to_string()));
        assert_eq!(frame.line, Some(42));
        assert_eq!(frame.column, Some(10));
    }

    #[test]
    fn test_backtrace_frame_format() {
        let frame = BacktraceFrame::new(
            Some("test_function".to_string()),
            Some("test.rs".to_string()),
            Some(42),
            None,
        );

        let formatted = frame.format();
        assert!(formatted.contains("test_function"));
        assert!(formatted.contains("test.rs"));
        assert!(formatted.contains("42"));
    }

    #[test]
    fn test_backtrace_capture() {
        let backtrace = Backtrace::capture();
        // Backtrace should always have at least one frame
        assert!(!backtrace.frames().is_empty());
    }

    #[test]
    fn test_backtrace_disabled() {
        let backtrace = Backtrace::disabled();
        assert!(backtrace.is_empty());
        assert!(!backtrace.enabled);
    }

    #[test]
    fn test_backtrace_format() {
        let backtrace = Backtrace::capture();
        let formatted = backtrace.format();
        assert!(!formatted.is_empty());
    }

    #[test]
    fn test_backtrace_format_limited() {
        let backtrace = Backtrace::capture();
        let formatted = backtrace.format_limited(5);
        assert!(!formatted.is_empty());
    }

    #[test]
    fn test_error_with_backtrace() {
        let error = ErrorWithBacktrace::new("test error");
        assert_eq!(error.message(), "test error");
        // Backtrace may or may not be enabled
        let _ = error.backtrace().frames();
    }

    #[test]
    fn test_error_without_backtrace() {
        let error = ErrorWithBacktrace::without_backtrace("test error");
        assert_eq!(error.message(), "test error");
        assert!(error.backtrace().is_empty());
    }

    #[test]
    fn test_error_format() {
        let error = ErrorWithBacktrace::new("test error");
        let formatted = error.format();
        assert!(formatted.contains("test error"));
    }

    #[test]
    fn test_backtrace_utils_capture() {
        let backtrace = BacktraceUtils::capture();
        // Backtrace should always have at least one frame (even if placeholder)
        assert!(!backtrace.frames().is_empty());
    }

    #[test]
    fn test_backtrace_utils_format() {
        let backtrace = Backtrace::capture();
        let formatted = BacktraceUtils::format(&backtrace);
        assert!(!formatted.is_empty());
    }

    #[test]
    fn test_backtrace_utils_format_limited() {
        let backtrace = Backtrace::capture();
        let formatted = BacktraceUtils::format_limited(&backtrace, 3);
        assert!(!formatted.is_empty());
    }

    #[test]
    fn test_backtrace_len() {
        let backtrace = Backtrace::capture();
        assert_eq!(backtrace.len(), backtrace.frames().len());
    }

    #[test]
    fn test_backtrace_is_empty() {
        let backtrace = Backtrace::disabled();
        assert!(backtrace.is_empty());
    }
}
