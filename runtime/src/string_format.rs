//! String formatting and interpolation support.

#![allow(dead_code)]

use std::fmt;

/// Format a string with arguments
pub struct StringFormatter;

impl StringFormatter {
    /// Format a string with positional arguments
    pub fn format(template: &str, args: &[&dyn fmt::Display]) -> String {
        let mut result = String::new();
        let mut chars = template.chars().peekable();
        let mut arg_index = 0;

        while let Some(ch) = chars.next() {
            if ch == '{' {
                if chars.peek() == Some(&'{') {
                    chars.next();
                    result.push('{');
                } else if chars.peek() == Some(&'}') {
                    chars.next();
                    if arg_index < args.len() {
                        result.push_str(&args[arg_index].to_string());
                        arg_index += 1;
                    }
                } else {
                    let mut index_str = String::new();
                    while let Some(&next_ch) = chars.peek() {
                        if next_ch == '}' {
                            chars.next();
                            break;
                        }
                        index_str.push(chars.next().unwrap());
                    }

                    if let Ok(index) = index_str.parse::<usize>() {
                        if index < args.len() {
                            result.push_str(&args[index].to_string());
                        }
                    }
                }
            } else if ch == '}' {
                if chars.peek() == Some(&'}') {
                    chars.next();
                    result.push('}');
                } else {
                    result.push(ch);
                }
            } else {
                result.push(ch);
            }
        }

        result
    }

    /// Format a string with named arguments
    pub fn format_named(template: &str, args: &[(&str, &dyn fmt::Display)]) -> String {
        let mut result = String::new();
        let mut chars = template.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '{' {
                if chars.peek() == Some(&'{') {
                    chars.next();
                    result.push('{');
                } else {
                    let mut name = String::new();
                    while let Some(&next_ch) = chars.peek() {
                        if next_ch == '}' {
                            chars.next();
                            break;
                        }
                        name.push(chars.next().unwrap());
                    }

                    if let Some((_, value)) = args.iter().find(|(n, _)| *n == name) {
                        result.push_str(&value.to_string());
                    }
                }
            } else if ch == '}' {
                if chars.peek() == Some(&'}') {
                    chars.next();
                    result.push('}');
                } else {
                    result.push(ch);
                }
            } else {
                result.push(ch);
            }
        }

        result
    }

    /// Format with padding
    pub fn format_padded(value: &dyn fmt::Display, width: usize, align: Alignment) -> String {
        let s = value.to_string();
        let len = s.len();

        if len >= width {
            return s;
        }

        let padding = width - len;
        match align {
            Alignment::Left => format!("{}{}", s, " ".repeat(padding)),
            Alignment::Right => format!("{}{}", " ".repeat(padding), s),
            Alignment::Center => {
                let left_pad = padding / 2;
                let right_pad = padding - left_pad;
                format!("{}{}{}", " ".repeat(left_pad), s, " ".repeat(right_pad))
            }
        }
    }

    /// Format a number with precision
    pub fn format_number(value: f64, precision: usize) -> String {
        format!("{value:.precision$}")
    }

    /// Format as hexadecimal
    pub fn format_hex(value: u64) -> String {
        format!("{value:x}")
    }

    /// Format as binary
    pub fn format_binary(value: u64) -> String {
        format!("{value:b}")
    }

    /// Format as octal
    pub fn format_octal(value: u64) -> String {
        format!("{value:o}")
    }
}

/// Alignment for formatted strings
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Alignment {
    Left,
    Right,
    Center,
}

/// String interpolation helper
pub struct StringInterpolator;

impl StringInterpolator {
    /// Interpolate variables into a string template
    pub fn interpolate(template: &str, vars: &[(&str, &str)]) -> String {
        let mut result = template.to_string();

        for (name, value) in vars {
            let placeholder = format!("${{{name}}}");
            result = result.replace(&placeholder, value);
        }

        result
    }

    /// Interpolate with expression evaluation
    pub fn interpolate_expr(template: &str, evaluator: &dyn Fn(&str) -> String) -> String {
        let mut result = String::new();
        let mut chars = template.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '$' && chars.peek() == Some(&'{') {
                chars.next();
                let mut expr = String::new();
                let mut brace_count = 1;

                for next_ch in chars.by_ref() {
                    if next_ch == '{' {
                        brace_count += 1;
                        expr.push(next_ch);
                    } else if next_ch == '}' {
                        brace_count -= 1;
                        if brace_count == 0 {
                            break;
                        }
                        expr.push(next_ch);
                    } else {
                        expr.push(next_ch);
                    }
                }

                result.push_str(&evaluator(&expr));
            } else {
                result.push(ch);
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_positional() {
        let args: Vec<&dyn fmt::Display> = vec![&"world", &42];
        let result = StringFormatter::format("Hello, {}! The answer is {}.", &args);
        assert_eq!(result, "Hello, world! The answer is 42.");
    }

    #[test]
    fn test_format_indexed() {
        let args: Vec<&dyn fmt::Display> = vec![&"first", &"second"];
        let result = StringFormatter::format("{1} comes after {0}", &args);
        assert_eq!(result, "second comes after first");
    }

    #[test]
    fn test_format_escape_braces() {
        let args: Vec<&dyn fmt::Display> = vec![&"test"];
        let result = StringFormatter::format("{{escaped}} {}", &args);
        assert_eq!(result, "{escaped} test");
    }

    #[test]
    fn test_format_named() {
        let args: Vec<(&str, &dyn fmt::Display)> = vec![("name", &"Alice"), ("age", &30)];
        let result = StringFormatter::format_named("Name: {name}, Age: {age}", &args);
        assert_eq!(result, "Name: Alice, Age: 30");
    }

    #[test]
    fn test_format_padded_left() {
        let result = StringFormatter::format_padded(&"test", 10, Alignment::Left);
        assert_eq!(result, "test      ");
    }

    #[test]
    fn test_format_padded_right() {
        let result = StringFormatter::format_padded(&"test", 10, Alignment::Right);
        assert_eq!(result, "      test");
    }

    #[test]
    fn test_format_padded_center() {
        let result = StringFormatter::format_padded(&"test", 10, Alignment::Center);
        assert_eq!(result, "   test   ");
    }

    #[test]
    fn test_format_number() {
        let result = StringFormatter::format_number(123.456, 2);
        assert_eq!(result, "123.46");
    }

    #[test]
    fn test_format_hex() {
        let result = StringFormatter::format_hex(255);
        assert_eq!(result, "ff");
    }

    #[test]
    fn test_format_binary() {
        let result = StringFormatter::format_binary(5);
        assert_eq!(result, "101");
    }

    #[test]
    fn test_format_octal() {
        let result = StringFormatter::format_octal(8);
        assert_eq!(result, "10");
    }

    #[test]
    fn test_interpolate_simple() {
        let vars = vec![("name", "Bob"), ("city", "NYC")];
        let result = StringInterpolator::interpolate("Hello ${name} from ${city}!", &vars);
        assert_eq!(result, "Hello Bob from NYC!");
    }

    #[test]
    fn test_interpolate_expr() {
        let evaluator = |expr: &str| -> String {
            match expr {
                "2 + 2" => "4".to_string(),
                "name" => "Alice".to_string(),
                _ => expr.to_string(),
            }
        };

        let result =
            StringInterpolator::interpolate_expr("Result: ${2 + 2}, Name: ${name}", &evaluator);
        assert_eq!(result, "Result: 4, Name: Alice");
    }

    #[test]
    fn test_interpolate_nested_braces() {
        let evaluator = |expr: &str| -> String { expr.to_uppercase() };

        let result = StringInterpolator::interpolate_expr("Value: ${test}", &evaluator);
        assert_eq!(result, "Value: TEST");
    }
}
