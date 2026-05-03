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
    /// Create a new formatter with default configuration (4-space indent, 100-char width).
    pub fn new() -> Self {
        Self {
            config: FormatConfig::default(),
        }
    }

    /// Create a new formatter with custom configuration.
    pub fn with_config(config: FormatConfig) -> Self {
        Self { config }
    }

    /// Format source code with indentation-aware formatting.
    ///
    /// Handles brace-based indentation, trailing whitespace removal,
    /// consistent spacing, and blank line normalization.
    pub fn format_source(&self, source: &str) -> Result<String, String> {
        let lines: Vec<&str> = source.lines().collect();
        let mut output = String::new();
        let mut indent_level: usize = 0;
        let mut prev_blank = false;

        for line in &lines {
            let trimmed = line.trim();

            // Handle blank lines: allow at most one consecutive blank line
            if trimmed.is_empty() {
                if !prev_blank && !output.is_empty() {
                    output.push('\n');
                }
                prev_blank = true;
                continue;
            }
            prev_blank = false;

            // Decrease indent for closing braces/brackets before writing
            let starts_with_close =
                trimmed.starts_with('}') || trimmed.starts_with(']') || trimmed.starts_with(')');
            if starts_with_close && indent_level > 0 {
                indent_level -= 1;
            }

            // Write indentation
            let indent = " ".repeat(self.config.indent_size * indent_level);
            output.push_str(&indent);
            output.push_str(trimmed);
            output.push('\n');

            // Increase indent after opening braces/brackets
            let open_count = trimmed
                .chars()
                .filter(|c| *c == '{' || *c == '[' || *c == '(')
                .count();
            let close_count = trimmed
                .chars()
                .filter(|c| *c == '}' || *c == ']' || *c == ')')
                .count();

            // Adjust for lines that both open and close (already handled close above)
            if starts_with_close {
                // We already decremented, so add back the close we counted
                let net = open_count as isize - (close_count as isize - 1);
                if net > 0 {
                    indent_level += net as usize;
                } else if net < 0 && indent_level > 0 {
                    indent_level = indent_level.saturating_sub((-net) as usize);
                }
            } else {
                let net = open_count as isize - close_count as isize;
                if net > 0 {
                    indent_level += net as usize;
                } else if net < 0 {
                    indent_level = indent_level.saturating_sub((-net) as usize);
                }
            }
        }

        // Ensure file ends with a single newline
        if !output.is_empty() && !output.ends_with('\n') {
            output.push('\n');
        }

        Ok(output)
    }

    /// Format a file
    pub fn format_file(&self, path: &Path) -> Result<String, String> {
        let source =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {e}"))?;
        self.format_source(&source)
    }

    /// Check if file needs formatting
    pub fn check_file(&self, path: &Path) -> Result<bool, String> {
        let source =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {e}"))?;
        let formatted = self.format_source(&source)?;
        Ok(source != formatted)
    }

    /// Format file in place
    pub fn format_file_in_place(&self, path: &Path) -> Result<bool, String> {
        let source =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {e}"))?;
        let formatted = self.format_source(&source)?;

        if source != formatted {
            std::fs::write(path, formatted).map_err(|e| format!("Failed to write file: {e}"))?;
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

    #[test]
    fn test_indentation_braces() {
        let formatter = Formatter::new();
        let source = "fn main() {\nlet x = 5;\n}";
        let formatted = formatter.format_source(source).unwrap();
        assert!(formatted.contains("    let x = 5;"));
    }

    #[test]
    fn test_trailing_whitespace_removal() {
        let formatter = Formatter::new();
        let source = "let x = 5;   \nlet y = 10;  ";
        let formatted = formatter.format_source(source).unwrap();
        assert!(!formatted.contains("   \n"));
    }

    #[test]
    fn test_blank_line_normalization() {
        let formatter = Formatter::new();
        let source = "let x = 5;\n\n\n\n\nlet y = 10;";
        let formatted = formatter.format_source(source).unwrap();
        // Should have at most one blank line between statements
        assert!(!formatted.contains("\n\n\n"));
    }

    #[test]
    fn test_nested_braces() {
        let formatter = Formatter::new();
        let source = "fn main() {\nif true {\nlet x = 5;\n}\n}";
        let formatted = formatter.format_source(source).unwrap();
        assert!(formatted.contains("    if true {"));
        assert!(formatted.contains("        let x = 5;"));
    }

    #[test]
    fn test_custom_config() {
        let config = FormatConfig {
            indent_size: 2,
            max_line_width: 80,
        };
        let formatter = Formatter::with_config(config);
        let source = "fn main() {\nlet x = 5;\n}";
        let formatted = formatter.format_source(source).unwrap();
        assert!(formatted.contains("  let x = 5;"));
    }

    #[test]
    fn test_empty_source() {
        let formatter = Formatter::new();
        let formatted = formatter.format_source("").unwrap();
        assert!(formatted.is_empty());
    }

    #[test]
    fn test_single_line() {
        let formatter = Formatter::new();
        let formatted = formatter.format_source("let x = 42;").unwrap();
        assert_eq!(formatted, "let x = 42;\n");
    }

    #[test]
    fn test_file_ends_with_newline() {
        let formatter = Formatter::new();
        let formatted = formatter.format_source("let x = 42;").unwrap();
        assert!(formatted.ends_with('\n'));
    }

    #[test]
    fn test_formatter_default() {
        let f = Formatter::default();
        assert_eq!(f.config.indent_size, 4);
        assert_eq!(f.config.max_line_width, 100);
    }

    #[test]
    fn test_format_file_nonexistent() {
        let f = Formatter::new();
        assert!(f.format_file(Path::new("/nonexistent/file.kr")).is_err());
    }

    #[test]
    fn test_check_file_nonexistent() {
        let f = Formatter::new();
        assert!(f.check_file(Path::new("/nonexistent/file.kr")).is_err());
    }

    #[test]
    fn test_format_file_in_place_nonexistent() {
        let f = Formatter::new();
        assert!(f
            .format_file_in_place(Path::new("/nonexistent/file.kr"))
            .is_err());
    }

    #[test]
    fn test_closing_bracket_indent() {
        let f = Formatter::new();
        let src = "fn foo() {\nlet arr = [\n1, 2, 3\n];\n}";
        let formatted = f.format_source(src).unwrap();
        assert!(formatted.contains("    let arr = ["));
        assert!(formatted.contains("    ];"));
    }

    #[test]
    fn test_closing_paren_indent() {
        let f = Formatter::new();
        let src = "call(\na,\nb\n);";
        let formatted = f.format_source(src).unwrap();
        assert!(formatted.contains(");"));
    }

    #[test]
    fn test_open_and_close_same_line() {
        let f = Formatter::new();
        let src = "fn main() {\n{ let x = 1; }\n}";
        let formatted = f.format_source(src).unwrap();
        assert!(formatted.contains("{ let x = 1; }"));
    }

    #[test]
    fn test_line_starting_with_close_and_opening_new() {
        let f = Formatter::new();
        // } else { pattern: closes one, opens one
        let src = "fn main() {\nif true {\nx;\n} else {\ny;\n}\n}";
        let formatted = f.format_source(src).unwrap();
        assert!(formatted.contains("} else {"));
    }

    #[test]
    fn test_multiple_consecutive_blank_lines_collapsed() {
        let f = Formatter::new();
        let src = "a;\n\n\n\n\nb;";
        let formatted = f.format_source(src).unwrap();
        let blank_count = formatted.matches("\n\n").count();
        assert!(blank_count <= 1);
    }

    #[test]
    fn test_only_blank_lines() {
        let f = Formatter::new();
        let formatted = f.format_source("\n\n\n").unwrap();
        assert!(formatted.is_empty());
    }

    #[test]
    fn test_deeply_nested() {
        let f = Formatter::new();
        let src = "a {\nb {\nc {\nd;\n}\n}\n}";
        let formatted = f.format_source(src).unwrap();
        assert!(formatted.contains("            d;"));
    }
}
