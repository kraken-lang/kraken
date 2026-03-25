use super::error::{CompilerError, ErrorCode, SourceLocation};

/// Create an undefined variable error with suggestions.
pub fn undefined_variable_error(
    location: SourceLocation,
    name: &str,
    suggestions: Vec<String>,
) -> CompilerError {
    let mut message = format!("undefined variable `{name}`");
    if !suggestions.is_empty() {
        message.push_str(&format!("\n  help: did you mean `{}`?", suggestions[0]));
        if suggestions.len() > 1 {
            message.push_str(&format!("\n        or `{}`?", suggestions[1]));
        }
    }
    CompilerError::TypeError {
        code: ErrorCode::E0008,
        location,
        message,
    }
}

/// Create an undefined function error with suggestions.
pub fn undefined_function_error(
    location: SourceLocation,
    name: &str,
    suggestions: Vec<String>,
) -> CompilerError {
    let mut message = format!("undefined function `{name}`");
    if !suggestions.is_empty() {
        message.push_str(&format!("\n  help: did you mean `{}`?", suggestions[0]));
        if suggestions.len() > 1 {
            message.push_str(&format!("\n        or `{}`?", suggestions[1]));
        }
    }
    CompilerError::TypeError {
        code: ErrorCode::E0009,
        location,
        message,
    }
}

/// Create a type mismatch error with expected and actual types.
pub fn type_mismatch_error(
    location: SourceLocation,
    expected: &str,
    actual: &str,
) -> CompilerError {
    CompilerError::TypeError {
        code: ErrorCode::E0007,
        location,
        message: format!("type mismatch: expected `{expected}`, found `{actual}`"),
    }
}

/// Calculate Levenshtein distance for "did you mean" suggestions.
pub fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_len = a.chars().count();
    let b_len = b.chars().count();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row = vec![0; b_len + 1];

    for (i, a_char) in a.chars().enumerate() {
        curr_row[0] = i + 1;

        for (j, b_char) in b.chars().enumerate() {
            let cost = if a_char == b_char { 0 } else { 1 };
            curr_row[j + 1] = (curr_row[j] + 1)
                .min(prev_row[j + 1] + 1)
                .min(prev_row[j] + cost);
        }

        std::mem::swap(&mut prev_row, &mut curr_row);
    }

    prev_row[b_len]
}

/// Find similar names for "did you mean" suggestions.
pub fn find_similar_names(
    target: &str,
    candidates: &[String],
    max_suggestions: usize,
) -> Vec<String> {
    let mut scored: Vec<(usize, String)> = candidates
        .iter()
        .map(|name| (levenshtein_distance(target, name), name.clone()))
        .filter(|(dist, _)| *dist <= 3) // Only suggest if distance <= 3
        .collect();

    scored.sort_by_key(|(dist, _)| *dist);
    scored.truncate(max_suggestions);
    scored.into_iter().map(|(_, name)| name).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("", ""), 0);
        assert_eq!(levenshtein_distance("a", ""), 1);
        assert_eq!(levenshtein_distance("", "a"), 1);
        assert_eq!(levenshtein_distance("abc", "abc"), 0);
        assert_eq!(levenshtein_distance("abc", "abd"), 1);
        assert_eq!(levenshtein_distance("kitten", "sitting"), 3);
    }

    #[test]
    fn test_find_similar_names() {
        let candidates = vec![
            "println".to_string(),
            "print".to_string(),
            "format".to_string(),
            "write".to_string(),
        ];

        let suggestions = find_similar_names("printl", &candidates, 2);
        assert!(suggestions.contains(&"println".to_string()));
        assert!(suggestions.contains(&"print".to_string()));
        assert_eq!(suggestions.len(), 2);
    }

    #[test]
    fn test_undefined_variable_error_with_suggestions() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 10, 5);
        let suggestions = vec!["count".to_string(), "counter".to_string()];
        let err = undefined_variable_error(loc, "cont", suggestions);

        let msg = err.to_string();
        assert!(msg.contains("undefined variable `cont`"));
        assert!(msg.contains("did you mean `count`?"));
    }

    #[test]
    fn test_type_mismatch_error() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 15, 8);
        let err = type_mismatch_error(loc, "i64", "string");

        let msg = err.to_string();
        assert!(msg.contains("type mismatch"));
        assert!(msg.contains("expected `i64`"));
        assert!(msg.contains("found `string`"));
    }

    #[test]
    fn test_undefined_variable_no_suggestions() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 1, 1);
        let err = undefined_variable_error(loc, "xyz", vec![]);
        let msg = err.to_string();
        assert!(msg.contains("undefined variable `xyz`"));
        assert!(!msg.contains("did you mean"));
    }

    #[test]
    fn test_undefined_function_with_suggestions() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 1, 1);
        let suggestions = vec!["foo".to_string(), "foobar".to_string()];
        let err = undefined_function_error(loc, "fo", suggestions);
        let msg = err.to_string();
        assert!(msg.contains("undefined function `fo`"));
        assert!(msg.contains("did you mean `foo`?"));
        assert!(msg.contains("or `foobar`?"));
    }

    #[test]
    fn test_undefined_function_no_suggestions() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 1, 1);
        let err = undefined_function_error(loc, "xyz", vec![]);
        let msg = err.to_string();
        assert!(msg.contains("undefined function `xyz`"));
        assert!(!msg.contains("did you mean"));
    }

    #[test]
    fn test_undefined_function_one_suggestion() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 1, 1);
        let err = undefined_function_error(loc, "fo", vec!["foo".to_string()]);
        let msg = err.to_string();
        assert!(msg.contains("did you mean `foo`?"));
        assert!(!msg.contains("or `"));
    }

    #[test]
    fn test_find_similar_names_no_matches() {
        let candidates = vec!["aaaaa".to_string(), "bbbbb".to_string()];
        let suggestions = find_similar_names("zzzzz", &candidates, 2);
        assert!(suggestions.is_empty());
    }

    #[test]
    fn test_levenshtein_identical_long() {
        assert_eq!(levenshtein_distance("abcdefgh", "abcdefgh"), 0);
    }

    #[test]
    fn test_levenshtein_completely_different() {
        assert_eq!(levenshtein_distance("abc", "xyz"), 3);
    }

    #[test]
    fn test_undefined_variable_one_suggestion() {
        let loc = SourceLocation::new(PathBuf::from("test.kr"), 1, 1);
        let err = undefined_variable_error(loc, "cnt", vec!["count".to_string()]);
        let msg = err.to_string();
        assert!(msg.contains("did you mean `count`?"));
        assert!(!msg.contains("or `"));
    }
}
