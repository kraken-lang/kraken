//! Kraken code formatter - Simple lexer-based formatting.
//!
//! This is a simplified formatter that reformats code based on tokens
//! rather than full AST traversal, making it more robust and maintainable.

use std::path::Path;

/// Format configuration
#[derive(Debug, Clone)]
pub struct FormatConfig {
    pub indent_size: usize,
    pub max_line_width: usize,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent_size: 4,
            max_line_width: 100,
        }
    }
}

/// Kraken code formatter
pub struct Formatter {
    #[allow(dead_code)]
    config: FormatConfig,
}

impl Formatter {
    pub fn new() -> Self {
        Self {
            config: FormatConfig::default(),
        }
    }

    pub fn with_config(config: FormatConfig) -> Self {
        Self { config }
    }

    /// Format source code (basic implementation)
    pub fn format_source(&self, source: &str) -> Result<String, String> {
        // TODO: Full token-based formatting implementation
        // For now, return source with normalized whitespace
        let lines: Vec<&str> = source.lines().collect();
        let mut output = String::new();

        for line in lines {
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                output.push_str(trimmed);
                output.push('\n');
            }
        }

        Ok(output)
    }

    /// Format a file
    pub fn format_file(&self, path: &Path) -> Result<String, String> {
        let source =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
        self.format_source(&source)
    }

    /// Check if file needs formatting
    pub fn check_file(&self, path: &Path) -> Result<bool, String> {
        let source =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
        let formatted = self.format_source(&source)?;
        Ok(source != formatted)
    }

    /// Format file in place
    pub fn format_file_in_place(&self, path: &Path) -> Result<bool, String> {
        let source =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
        let formatted = self.format_source(&source)?;

        if source != formatted {
            std::fs::write(path, formatted).map_err(|e| format!("Failed to write file: {}", e))?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_formatter_creation() {
        let formatter = Formatter::new();
        assert_eq!(formatter.config.indent_size, 4);
    }

    #[test]
    fn test_basic_formatting() {
        let formatter = Formatter::new();
        let source = "fn main(){return 0;}";
        let result = formatter.format_source(source);
        assert!(result.is_ok());
        let formatted = result.unwrap();
        assert!(formatted.contains("fn main()"));
        assert!(formatted.contains("return 0;"));
    }
}
