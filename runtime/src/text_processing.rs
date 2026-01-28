//! Text processing module providing regex, Unicode normalization, and segmentation.

#![allow(dead_code)]

use regex::Regex;
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

/// Regular expression wrapper
pub struct RegexWrapper {
    inner: Regex,
}

impl RegexWrapper {
    /// Create a new regex from a pattern
    pub fn new(pattern: &str) -> Result<Self, String> {
        Regex::new(pattern)
            .map(|inner| Self { inner })
            .map_err(|e| e.to_string())
    }

    /// Check if the regex matches the text
    pub fn is_match(&self, text: &str) -> bool {
        self.inner.is_match(text)
    }

    /// Find the first match in the text
    pub fn find(&self, text: &str) -> Option<String> {
        self.inner.find(text).map(|m| m.as_str().to_string())
    }

    /// Find all matches in the text
    pub fn find_all(&self, text: &str) -> Vec<String> {
        self.inner
            .find_iter(text)
            .map(|m| m.as_str().to_string())
            .collect()
    }

    /// Replace the first match with replacement
    pub fn replace(&self, text: &str, replacement: &str) -> String {
        self.inner.replace(text, replacement).to_string()
    }

    /// Replace all matches with replacement
    pub fn replace_all(&self, text: &str, replacement: &str) -> String {
        self.inner.replace_all(text, replacement).to_string()
    }

    /// Split text by the regex pattern
    pub fn split(&self, text: &str) -> Vec<String> {
        self.inner.split(text).map(|s| s.to_string()).collect()
    }

    /// Capture groups from the first match
    pub fn captures(&self, text: &str) -> Option<Vec<String>> {
        self.inner.captures(text).map(|caps| {
            caps.iter()
                .filter_map(|m| m.map(|m| m.as_str().to_string()))
                .collect()
        })
    }
}

/// Unicode normalization utilities
pub struct UnicodeNormalizer;

impl UnicodeNormalizer {
    /// Normalize to NFC (Canonical Decomposition, followed by Canonical Composition)
    pub fn nfc(text: &str) -> String {
        text.nfc().collect()
    }

    /// Normalize to NFD (Canonical Decomposition)
    pub fn nfd(text: &str) -> String {
        text.nfd().collect()
    }

    /// Normalize to NFKC (Compatibility Decomposition, followed by Canonical Composition)
    pub fn nfkc(text: &str) -> String {
        text.nfkc().collect()
    }

    /// Normalize to NFKD (Compatibility Decomposition)
    pub fn nfkd(text: &str) -> String {
        text.nfkd().collect()
    }
}

/// Unicode segmentation utilities
pub struct UnicodeSegmenter;

impl UnicodeSegmenter {
    /// Segment text into grapheme clusters
    pub fn graphemes(text: &str) -> Vec<String> {
        text.graphemes(true).map(|s| s.to_string()).collect()
    }

    /// Segment text into words
    pub fn words(text: &str) -> Vec<String> {
        text.unicode_words().map(|s| s.to_string()).collect()
    }

    /// Segment text into sentences (simple implementation)
    pub fn sentences(text: &str) -> Vec<String> {
        text.split_sentence_bounds()
            .map(|s| s.to_string())
            .collect()
    }

    /// Count grapheme clusters
    pub fn grapheme_count(text: &str) -> usize {
        text.graphemes(true).count()
    }

    /// Count words
    pub fn word_count(text: &str) -> usize {
        text.unicode_words().count()
    }
}

/// Case folding and mapping utilities
pub struct CaseMapper;

impl CaseMapper {
    /// Convert to uppercase
    pub fn to_uppercase(text: &str) -> String {
        text.to_uppercase()
    }

    /// Convert to lowercase
    pub fn to_lowercase(text: &str) -> String {
        text.to_lowercase()
    }

    /// Case fold for case-insensitive comparison
    pub fn case_fold(text: &str) -> String {
        text.to_lowercase()
    }

    /// Title case (capitalize first letter of each word)
    pub fn to_titlecase(text: &str) -> String {
        text.unicode_words()
            .map(|word| {
                let mut chars = word.chars();
                match chars.next() {
                    None => String::new(),
                    Some(first) => {
                        first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase()
                    }
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Check if text is uppercase
    pub fn is_uppercase(text: &str) -> bool {
        text.chars().all(|c| !c.is_lowercase())
    }

    /// Check if text is lowercase
    pub fn is_lowercase(text: &str) -> bool {
        text.chars().all(|c| !c.is_uppercase())
    }
}

/// Advanced string utilities
pub struct AdvancedStringUtils;

impl AdvancedStringUtils {
    /// Wrap text to specified width
    pub fn wrap(text: &str, width: usize) -> Vec<String> {
        let mut lines = Vec::new();
        let mut current_line = String::new();
        let mut current_width = 0;

        for word in text.split_whitespace() {
            let word_len = word.len();
            if current_width + word_len + 1 > width && !current_line.is_empty() {
                lines.push(current_line.clone());
                current_line.clear();
                current_width = 0;
            }
            if !current_line.is_empty() {
                current_line.push(' ');
                current_width += 1;
            }
            current_line.push_str(word);
            current_width += word_len;
        }
        if !current_line.is_empty() {
            lines.push(current_line);
        }
        lines
    }

    /// Truncate text to specified length with ellipsis
    pub fn truncate(text: &str, max_len: usize) -> String {
        if text.len() <= max_len {
            text.to_string()
        } else {
            format!("{}...", &text[..max_len.saturating_sub(3)])
        }
    }

    /// Indent text by specified number of spaces
    pub fn indent(text: &str, spaces: usize) -> String {
        let indent = " ".repeat(spaces);
        text.lines()
            .map(|line| format!("{indent}{line}"))
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Remove common leading whitespace from all lines
    pub fn dedent(text: &str) -> String {
        let lines: Vec<&str> = text.lines().collect();
        if lines.is_empty() {
            return String::new();
        }

        let min_indent = lines
            .iter()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.len() - line.trim_start().len())
            .min()
            .unwrap_or(0);

        lines
            .iter()
            .map(|line| {
                if line.len() >= min_indent {
                    &line[min_indent..]
                } else {
                    line
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Center text within specified width
    pub fn center(text: &str, width: usize) -> String {
        let text_len = text.len();
        if text_len >= width {
            return text.to_string();
        }
        let padding = width - text_len;
        let left_pad = padding / 2;
        let right_pad = padding - left_pad;
        format!("{}{}{}", " ".repeat(left_pad), text, " ".repeat(right_pad))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_is_match() {
        let re = RegexWrapper::new(r"\d+").unwrap();
        assert!(re.is_match("abc123"));
        assert!(!re.is_match("abc"));
    }

    #[test]
    fn test_regex_find() {
        let re = RegexWrapper::new(r"\d+").unwrap();
        assert_eq!(re.find("abc123def"), Some("123".to_string()));
    }

    #[test]
    fn test_regex_find_all() {
        let re = RegexWrapper::new(r"\d+").unwrap();
        let matches = re.find_all("abc123def456");
        assert_eq!(matches, vec!["123", "456"]);
    }

    #[test]
    fn test_regex_replace() {
        let re = RegexWrapper::new(r"\d+").unwrap();
        assert_eq!(re.replace("abc123def", "X"), "abcXdef");
    }

    #[test]
    fn test_regex_replace_all() {
        let re = RegexWrapper::new(r"\d+").unwrap();
        assert_eq!(re.replace_all("abc123def456", "X"), "abcXdefX");
    }

    #[test]
    fn test_regex_split() {
        let re = RegexWrapper::new(r"\s+").unwrap();
        let parts = re.split("hello  world\ttab");
        assert_eq!(parts, vec!["hello", "world", "tab"]);
    }

    #[test]
    fn test_regex_captures() {
        let re = RegexWrapper::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap();
        let caps = re.captures("2024-01-28").unwrap();
        assert_eq!(caps.len(), 4);
        assert_eq!(caps[0], "2024-01-28");
        assert_eq!(caps[1], "2024");
        assert_eq!(caps[2], "01");
        assert_eq!(caps[3], "28");
    }

    #[test]
    fn test_unicode_nfc() {
        let text = "é";
        let normalized = UnicodeNormalizer::nfc(text);
        assert!(!normalized.is_empty());
    }

    #[test]
    fn test_unicode_nfd() {
        let text = "é";
        let normalized = UnicodeNormalizer::nfd(text);
        assert!(!normalized.is_empty());
    }

    #[test]
    fn test_unicode_graphemes() {
        let text = "hello";
        let graphemes = UnicodeSegmenter::graphemes(text);
        assert_eq!(graphemes.len(), 5);
    }

    #[test]
    fn test_unicode_words() {
        let text = "hello world";
        let words = UnicodeSegmenter::words(text);
        assert_eq!(words, vec!["hello", "world"]);
    }

    #[test]
    fn test_unicode_word_count() {
        let text = "hello world foo bar";
        assert_eq!(UnicodeSegmenter::word_count(text), 4);
    }

    #[test]
    fn test_case_mapper_uppercase() {
        assert_eq!(CaseMapper::to_uppercase("hello"), "HELLO");
    }

    #[test]
    fn test_case_mapper_lowercase() {
        assert_eq!(CaseMapper::to_lowercase("HELLO"), "hello");
    }

    #[test]
    fn test_case_mapper_titlecase() {
        assert_eq!(CaseMapper::to_titlecase("hello world"), "Hello World");
    }

    #[test]
    fn test_case_mapper_is_uppercase() {
        assert!(CaseMapper::is_uppercase("HELLO"));
        assert!(!CaseMapper::is_uppercase("Hello"));
    }

    #[test]
    fn test_case_mapper_is_lowercase() {
        assert!(CaseMapper::is_lowercase("hello"));
        assert!(!CaseMapper::is_lowercase("Hello"));
    }

    #[test]
    fn test_string_wrap() {
        let text = "hello world foo bar";
        let wrapped = AdvancedStringUtils::wrap(text, 10);
        assert_eq!(wrapped.len(), 3);
    }

    #[test]
    fn test_string_truncate() {
        let text = "hello world";
        assert_eq!(AdvancedStringUtils::truncate(text, 8), "hello...");
        assert_eq!(AdvancedStringUtils::truncate(text, 20), "hello world");
    }

    #[test]
    fn test_string_indent() {
        let text = "hello\nworld";
        let indented = AdvancedStringUtils::indent(text, 2);
        assert_eq!(indented, "  hello\n  world");
    }

    #[test]
    fn test_string_dedent() {
        let text = "  hello\n  world";
        let dedented = AdvancedStringUtils::dedent(text);
        assert_eq!(dedented, "hello\nworld");
    }

    #[test]
    fn test_string_center() {
        let text = "hello";
        let centered = AdvancedStringUtils::center(text, 11);
        assert_eq!(centered, "   hello   ");
    }
}
