//! Core testing assertion primitives for Kraken.

#![allow(dead_code)]

use std::fmt::Debug;

/// Assert that two values are equal
pub fn assert_eq<T: PartialEq + Debug>(left: T, right: T, message: Option<&str>) {
    if left != right {
        let msg = message.unwrap_or("assertion failed: `(left == right)`");
        panic!(
            "{msg}
  left: `{left:?}`,
 right: `{right:?}`"
        );
    }
}

/// Assert that two values are not equal
pub fn assert_ne<T: PartialEq + Debug>(left: T, right: T, message: Option<&str>) {
    if left == right {
        let msg = message.unwrap_or("assertion failed: `(left != right)`");
        panic!(
            "{msg}
  left: `{left:?}`,
 right: `{right:?}`"
        );
    }
}

/// Assert that a condition is true
pub fn assert(condition: bool, message: Option<&str>) {
    if !condition {
        let msg = message.unwrap_or("assertion failed");
        panic!("{msg}");
    }
}

/// Assert that two floating point values are approximately equal
pub fn assert_approx_eq(left: f64, right: f64, epsilon: f64, message: Option<&str>) {
    if (left - right).abs() > epsilon {
        let msg = message.unwrap_or("assertion failed: `(left ≈ right)`");
        panic!("{msg}\n  left: `{left}`,\n right: `{right}`,\n epsilon: `{epsilon}`");
    }
}

/// Assert that a value matches a pattern
pub fn assert_matches<T, F>(value: T, predicate: F, message: Option<&str>)
where
    F: FnOnce(&T) -> bool,
{
    if !predicate(&value) {
        let msg = message.unwrap_or("assertion failed: value does not match predicate");
        panic!("{msg}");
    }
}

/// Assertion utilities
pub struct Assertions;

impl Assertions {
    /// Assert equality with custom message
    pub fn eq<T: PartialEq + Debug>(left: T, right: T, message: &str) {
        assert_eq(left, right, Some(message));
    }

    /// Assert inequality with custom message
    pub fn ne<T: PartialEq + Debug>(left: T, right: T, message: &str) {
        assert_ne(left, right, Some(message));
    }

    /// Assert condition with custom message
    pub fn is_true(condition: bool, message: &str) {
        assert(condition, Some(message));
    }

    /// Assert condition is false with custom message
    pub fn is_false(condition: bool, message: &str) {
        assert(!condition, Some(message));
    }

    /// Assert value is Some
    pub fn is_some<T>(value: Option<T>, message: &str) {
        if value.is_none() {
            panic!("{message}");
        }
    }

    /// Assert value is None
    pub fn is_none<T: Debug>(value: Option<T>, message: &str) {
        if let Some(v) = value {
            panic!(
                "{message}
  value: `{v:?}`"
            );
        }
    }

    /// Assert Result is Ok
    pub fn is_ok<T, E: Debug>(value: Result<T, E>, message: &str) {
        if let Err(e) = value {
            panic!(
                "{message}
  error: `{e:?}`"
            );
        }
    }

    /// Assert Result is Err
    pub fn is_err<T: Debug, E>(value: Result<T, E>, message: &str) {
        if let Ok(v) = value {
            panic!(
                "{message}
  value: `{v:?}`"
            );
        }
    }

    /// Assert collection contains element
    pub fn contains<T: PartialEq + Debug>(collection: &[T], element: &T, message: &str) {
        if !collection.contains(element) {
            panic!(
                "{message}
  element: `{element:?}` not found in collection"
            );
        }
    }

    /// Assert collection does not contain element
    pub fn not_contains<T: PartialEq + Debug>(collection: &[T], element: &T, message: &str) {
        if collection.contains(element) {
            panic!(
                "{message}
  element: `{element:?}` found in collection"
            );
        }
    }

    /// Assert collection is empty
    pub fn is_empty<T>(collection: &[T], message: &str) {
        if !collection.is_empty() {
            let len = collection.len();
            panic!(
                "{message}
  collection length: `{len}"
            );
        }
    }

    /// Assert collection is not empty
    pub fn not_empty<T>(collection: &[T], message: &str) {
        if collection.is_empty() {
            panic!("{}", message);
        }
    }

    /// Assert two collections have the same length
    pub fn same_length<T, U>(left: &[T], right: &[U], message: &str) {
        if left.len() != right.len() {
            let left_len = left.len();
            let right_len = right.len();
            panic!(
                "{message}
  left length: `{left_len}`,
 right length: `{right_len}"
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assert_eq_success() {
        assert_eq(1, 1, None);
        assert_eq("hello", "hello", None);
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn test_assert_eq_failure() {
        assert_eq(1, 2, None);
    }

    #[test]
    fn test_assert_ne_success() {
        assert_ne(1, 2, None);
        assert_ne("hello", "world", None);
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn test_assert_ne_failure() {
        assert_ne(1, 1, None);
    }

    #[test]
    fn test_assert_success() {
        assert(true, None);
        assert(1 + 1 == 2, None);
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn test_assert_failure() {
        assert(false, None);
    }

    #[test]
    fn test_assert_approx_eq_success() {
        assert_approx_eq(1.0, 1.0001, 0.001, None);
        assert_approx_eq(2.71828, 2.71829, 0.0001, None);
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn test_assert_approx_eq_failure() {
        assert_approx_eq(1.0, 2.0, 0.1, None);
    }

    #[test]
    fn test_assert_matches_success() {
        assert_matches(5, |x| *x > 0, None);
        assert_matches("hello", |s| !s.is_empty(), None);
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn test_assert_matches_failure() {
        assert_matches(5, |x| *x < 0, None);
    }

    #[test]
    fn test_assertions_is_some() {
        Assertions::is_some(Some(42), "value should be Some");
    }

    #[test]
    #[should_panic]
    fn test_assertions_is_some_failure() {
        Assertions::is_some(None::<i32>, "value should be Some");
    }

    #[test]
    fn test_assertions_is_none() {
        Assertions::is_none(None::<i32>, "value should be None");
    }

    #[test]
    #[should_panic]
    fn test_assertions_is_none_failure() {
        Assertions::is_none(Some(42), "value should be None");
    }

    #[test]
    fn test_assertions_is_ok() {
        Assertions::is_ok(Ok::<i32, String>(42), "result should be Ok");
    }

    #[test]
    #[should_panic]
    fn test_assertions_is_ok_failure() {
        Assertions::is_ok(
            Err::<i32, String>("error".to_string()),
            "result should be Ok",
        );
    }

    #[test]
    fn test_assertions_is_err() {
        Assertions::is_err(
            Err::<i32, String>("error".to_string()),
            "result should be Err",
        );
    }

    #[test]
    #[should_panic]
    fn test_assertions_is_err_failure() {
        Assertions::is_err(Ok::<i32, String>(42), "result should be Err");
    }

    #[test]
    fn test_assertions_contains() {
        let vec = vec![1, 2, 3, 4, 5];
        Assertions::contains(&vec, &3, "should contain 3");
    }

    #[test]
    #[should_panic]
    fn test_assertions_contains_failure() {
        let vec = vec![1, 2, 3, 4, 5];
        Assertions::contains(&vec, &10, "should contain 10");
    }

    #[test]
    fn test_assertions_not_contains() {
        let vec = vec![1, 2, 3, 4, 5];
        Assertions::not_contains(&vec, &10, "should not contain 10");
    }

    #[test]
    #[should_panic]
    fn test_assertions_not_contains_failure() {
        let vec = vec![1, 2, 3, 4, 5];
        Assertions::not_contains(&vec, &3, "should not contain 3");
    }

    #[test]
    fn test_assertions_is_empty() {
        let vec: Vec<i32> = vec![];
        Assertions::is_empty(&vec, "should be empty");
    }

    #[test]
    #[should_panic]
    fn test_assertions_is_empty_failure() {
        let vec = vec![1, 2, 3];
        Assertions::is_empty(&vec, "should be empty");
    }

    #[test]
    fn test_assertions_not_empty() {
        let vec = vec![1, 2, 3];
        Assertions::not_empty(&vec, "should not be empty");
    }

    #[test]
    #[should_panic]
    fn test_assertions_not_empty_failure() {
        let vec: Vec<i32> = vec![];
        Assertions::not_empty(&vec, "should not be empty");
    }

    #[test]
    fn test_assertions_same_length() {
        let vec1 = vec![1, 2, 3];
        let vec2 = vec!["a", "b", "c"];
        Assertions::same_length(&vec1, &vec2, "should have same length");
    }

    #[test]
    #[should_panic]
    fn test_assertions_same_length_failure() {
        let vec1 = vec![1, 2, 3];
        let vec2 = vec!["a", "b"];
        Assertions::same_length(&vec1, &vec2, "should have same length");
    }
}
