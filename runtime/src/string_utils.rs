//! String manipulation utilities for Kraken runtime.
//!
//! Provides common string operations and transformations.

/// Split a string by a delimiter.
pub fn split_string(s: &str, delimiter: &str) -> Vec<String> {
    s.split(delimiter).map(|s| s.to_string()).collect()
}

/// Join strings with a separator.
pub fn join_strings(strings: &[String], separator: &str) -> String {
    strings.join(separator)
}

/// Trim whitespace from both ends of a string.
pub fn trim_string(s: &str) -> String {
    s.trim().to_string()
}

/// Convert string to uppercase.
pub fn to_uppercase(s: &str) -> String {
    s.to_uppercase()
}

/// Convert string to lowercase.
pub fn to_lowercase(s: &str) -> String {
    s.to_lowercase()
}

/// Check if string starts with a prefix.
pub fn starts_with(s: &str, prefix: &str) -> bool {
    s.starts_with(prefix)
}

/// Check if string ends with a suffix.
pub fn ends_with(s: &str, suffix: &str) -> bool {
    s.ends_with(suffix)
}

/// Replace all occurrences of a pattern with a replacement.
pub fn replace_all(s: &str, pattern: &str, replacement: &str) -> String {
    s.replace(pattern, replacement)
}

/// Check if string contains a substring.
pub fn contains_substring(s: &str, substring: &str) -> bool {
    s.contains(substring)
}

/// Repeat a string n times.
pub fn repeat_string(s: &str, n: usize) -> String {
    s.repeat(n)
}

/// Reverse a string.
pub fn reverse_string(s: &str) -> String {
    s.chars().rev().collect()
}

/// Get substring by character indices.
pub fn substring(s: &str, start: usize, end: usize) -> String {
    s.chars()
        .skip(start)
        .take(end.saturating_sub(start))
        .collect()
}

/// Pad string to the left with a character.
pub fn pad_left(s: &str, width: usize, pad_char: char) -> String {
    let current_len = s.chars().count();
    if current_len >= width {
        s.to_string()
    } else {
        let padding = pad_char.to_string().repeat(width - current_len);
        format!("{padding}{s}")
    }
}

/// Pad string to the right with a character.
pub fn pad_right(s: &str, width: usize, pad_char: char) -> String {
    let current_len = s.chars().count();
    if current_len >= width {
        s.to_string()
    } else {
        let padding = pad_char.to_string().repeat(width - current_len);
        format!("{s}{padding}")
    }
}

/// Count occurrences of a substring.
pub fn count_occurrences(s: &str, pattern: &str) -> usize {
    s.matches(pattern).count()
}

/// Check if string is empty or only whitespace.
pub fn is_blank(s: &str) -> bool {
    s.trim().is_empty()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_string() {
        let result = split_string("a,b,c", ",");
        assert_eq!(result, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_join_strings() {
        let strings = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        assert_eq!(join_strings(&strings, ","), "a,b,c");
    }

    #[test]
    fn test_trim_string() {
        assert_eq!(trim_string("  hello  "), "hello");
    }

    #[test]
    fn test_case_conversion() {
        assert_eq!(to_uppercase("hello"), "HELLO");
        assert_eq!(to_lowercase("HELLO"), "hello");
    }

    #[test]
    fn test_starts_ends_with() {
        assert!(starts_with("hello world", "hello"));
        assert!(ends_with("hello world", "world"));
        assert!(!starts_with("hello world", "world"));
    }

    #[test]
    fn test_replace_all() {
        assert_eq!(replace_all("hello world", "o", "0"), "hell0 w0rld");
    }

    #[test]
    fn test_contains_substring() {
        assert!(contains_substring("hello world", "lo wo"));
        assert!(!contains_substring("hello world", "xyz"));
    }

    #[test]
    fn test_repeat_string() {
        assert_eq!(repeat_string("ab", 3), "ababab");
    }

    #[test]
    fn test_reverse_string() {
        assert_eq!(reverse_string("hello"), "olleh");
    }

    #[test]
    fn test_substring() {
        assert_eq!(substring("hello world", 0, 5), "hello");
        assert_eq!(substring("hello world", 6, 11), "world");
    }

    #[test]
    fn test_pad_left() {
        assert_eq!(pad_left("42", 5, '0'), "00042");
        assert_eq!(pad_left("hello", 3, '0'), "hello");
    }

    #[test]
    fn test_pad_right() {
        assert_eq!(pad_right("42", 5, '0'), "42000");
        assert_eq!(pad_right("hello", 3, '0'), "hello");
    }

    #[test]
    fn test_count_occurrences() {
        assert_eq!(count_occurrences("hello world", "l"), 3);
        assert_eq!(count_occurrences("hello world", "o"), 2);
    }

    #[test]
    fn test_is_blank() {
        assert!(is_blank(""));
        assert!(is_blank("   "));
        assert!(!is_blank("hello"));
    }
}
