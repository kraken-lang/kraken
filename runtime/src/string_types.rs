//! Core string types: String, str utilities, and string operations.

#![allow(dead_code)]

use std::fmt;

/// String builder for efficient string concatenation
pub struct StringBuilder {
    buffer: String,
}

impl StringBuilder {
    /// Create a new empty string builder
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
        }
    }

    /// Create a string builder with initial capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buffer: String::with_capacity(capacity),
        }
    }

    /// Append a string slice
    pub fn append(&mut self, s: &str) -> &mut Self {
        self.buffer.push_str(s);
        self
    }

    /// Append a character
    pub fn append_char(&mut self, c: char) -> &mut Self {
        self.buffer.push(c);
        self
    }

    /// Append a formatted value
    pub fn append_fmt(&mut self, args: fmt::Arguments<'_>) -> &mut Self {
        use std::fmt::Write;
        let _ = write!(&mut self.buffer, "{args}");
        self
    }

    /// Get the length of the string
    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    /// Check if the string is empty
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    /// Clear the string builder
    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    /// Build the final string
    pub fn build(self) -> String {
        self.buffer
    }

    /// Get a reference to the current string
    pub fn as_str(&self) -> &str {
        &self.buffer
    }
}

impl Default for StringBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// String utilities for common operations
pub struct StringUtils;

impl StringUtils {
    /// Convert string to uppercase
    pub fn to_uppercase(s: &str) -> String {
        s.to_uppercase()
    }

    /// Convert string to lowercase
    pub fn to_lowercase(s: &str) -> String {
        s.to_lowercase()
    }

    /// Capitalize first letter
    pub fn capitalize(s: &str) -> String {
        let mut chars = s.chars();
        match chars.next() {
            None => String::new(),
            Some(first) => first.to_uppercase().chain(chars).collect(),
        }
    }

    /// Trim whitespace from both ends
    pub fn trim(s: &str) -> &str {
        s.trim()
    }

    /// Trim whitespace from start
    pub fn trim_start(s: &str) -> &str {
        s.trim_start()
    }

    /// Trim whitespace from end
    pub fn trim_end(s: &str) -> &str {
        s.trim_end()
    }

    /// Split string by delimiter
    pub fn split<'a>(s: &'a str, delimiter: &str) -> Vec<&'a str> {
        s.split(delimiter).collect()
    }

    /// Split string by whitespace
    pub fn split_whitespace(s: &str) -> Vec<&str> {
        s.split_whitespace().collect()
    }

    /// Join strings with delimiter
    pub fn join(parts: &[&str], delimiter: &str) -> String {
        parts.join(delimiter)
    }

    /// Check if string starts with prefix
    pub fn starts_with(s: &str, prefix: &str) -> bool {
        s.starts_with(prefix)
    }

    /// Check if string ends with suffix
    pub fn ends_with(s: &str, suffix: &str) -> bool {
        s.ends_with(suffix)
    }

    /// Check if string contains substring
    pub fn contains(s: &str, substring: &str) -> bool {
        s.contains(substring)
    }

    /// Replace all occurrences of pattern with replacement
    pub fn replace(s: &str, pattern: &str, replacement: &str) -> String {
        s.replace(pattern, replacement)
    }

    /// Repeat string n times
    pub fn repeat(s: &str, n: usize) -> String {
        s.repeat(n)
    }

    /// Pad string to length with character
    pub fn pad_left(s: &str, length: usize, pad_char: char) -> String {
        if s.len() >= length {
            s.to_string()
        } else {
            let padding = pad_char.to_string().repeat(length - s.len());
            format!("{padding}{s}")
        }
    }

    /// Pad string to length with character on right
    pub fn pad_right(s: &str, length: usize, pad_char: char) -> String {
        if s.len() >= length {
            s.to_string()
        } else {
            let padding = pad_char.to_string().repeat(length - s.len());
            format!("{s}{padding}")
        }
    }

    /// Reverse a string
    pub fn reverse(s: &str) -> String {
        s.chars().rev().collect()
    }

    /// Get substring by byte indices
    pub fn substring(s: &str, start: usize, end: usize) -> &str {
        &s[start..end]
    }

    /// Count occurrences of substring
    pub fn count_occurrences(s: &str, substring: &str) -> usize {
        s.matches(substring).count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_builder_new() {
        let builder = StringBuilder::new();
        assert!(builder.is_empty());
        assert_eq!(builder.len(), 0);
    }

    #[test]
    fn test_string_builder_append() {
        let mut builder = StringBuilder::new();
        builder.append("hello").append(" ").append("world");
        assert_eq!(builder.as_str(), "hello world");
    }

    #[test]
    fn test_string_builder_append_char() {
        let mut builder = StringBuilder::new();
        builder.append("test").append_char('!');
        assert_eq!(builder.as_str(), "test!");
    }

    #[test]
    fn test_string_builder_build() {
        let mut builder = StringBuilder::new();
        builder.append("hello");
        let result = builder.build();
        assert_eq!(result, "hello");
    }

    #[test]
    fn test_string_builder_clear() {
        let mut builder = StringBuilder::new();
        builder.append("test");
        assert!(!builder.is_empty());
        builder.clear();
        assert!(builder.is_empty());
    }

    #[test]
    fn test_to_uppercase() {
        assert_eq!(StringUtils::to_uppercase("hello"), "HELLO");
    }

    #[test]
    fn test_to_lowercase() {
        assert_eq!(StringUtils::to_lowercase("HELLO"), "hello");
    }

    #[test]
    fn test_capitalize() {
        assert_eq!(StringUtils::capitalize("hello"), "Hello");
        assert_eq!(StringUtils::capitalize(""), "");
    }

    #[test]
    fn test_trim() {
        assert_eq!(StringUtils::trim("  hello  "), "hello");
    }

    #[test]
    fn test_split() {
        let parts = StringUtils::split("a,b,c", ",");
        assert_eq!(parts, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_split_whitespace() {
        let parts = StringUtils::split_whitespace("hello world test");
        assert_eq!(parts, vec!["hello", "world", "test"]);
    }

    #[test]
    fn test_join() {
        let result = StringUtils::join(&["a", "b", "c"], ",");
        assert_eq!(result, "a,b,c");
    }

    #[test]
    fn test_starts_with() {
        assert!(StringUtils::starts_with("hello", "hel"));
        assert!(!StringUtils::starts_with("hello", "world"));
    }

    #[test]
    fn test_ends_with() {
        assert!(StringUtils::ends_with("hello", "llo"));
        assert!(!StringUtils::ends_with("hello", "world"));
    }

    #[test]
    fn test_contains() {
        assert!(StringUtils::contains("hello world", "world"));
        assert!(!StringUtils::contains("hello", "xyz"));
    }

    #[test]
    fn test_replace() {
        assert_eq!(
            StringUtils::replace("hello world", "world", "rust"),
            "hello rust"
        );
    }

    #[test]
    fn test_repeat() {
        assert_eq!(StringUtils::repeat("ab", 3), "ababab");
    }

    #[test]
    fn test_pad_left() {
        assert_eq!(StringUtils::pad_left("5", 3, '0'), "005");
        assert_eq!(StringUtils::pad_left("hello", 3, '0'), "hello");
    }

    #[test]
    fn test_pad_right() {
        assert_eq!(StringUtils::pad_right("5", 3, '0'), "500");
        assert_eq!(StringUtils::pad_right("hello", 3, '0'), "hello");
    }

    #[test]
    fn test_reverse() {
        assert_eq!(StringUtils::reverse("hello"), "olleh");
    }

    #[test]
    fn test_substring() {
        assert_eq!(StringUtils::substring("hello", 1, 4), "ell");
    }

    #[test]
    fn test_count_occurrences() {
        assert_eq!(
            StringUtils::count_occurrences("hello world hello", "hello"),
            2
        );
    }
}
