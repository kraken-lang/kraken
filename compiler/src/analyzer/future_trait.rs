//! Future trait infrastructure for async/await support.

use crate::error::{CompilerError, CompilerResult};
use crate::parser::ast::Type;
use std::collections::HashMap;

/// Future trait implementation tracker
pub struct FutureTracker {
    /// Track which types implement Future
    future_impls: HashMap<String, FutureImpl>,
}

/// Information about a Future trait implementation
#[derive(Debug, Clone)]
pub struct FutureImpl {
    pub type_name: String,
    pub output_type: Type,
}

/// Poll result for futures
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PollResult {
    Ready,
    Pending,
}

impl FutureTracker {
    /// Create a new future tracker with no registered implementations.
    pub fn new() -> Self {
        Self {
            future_impls: HashMap::new(),
        }
    }

    /// Register a Future trait implementation
    pub fn register_future_impl(&mut self, type_name: String, output_type: Type) {
        self.future_impls.insert(
            type_name.clone(),
            FutureImpl {
                type_name,
                output_type,
            },
        );
    }

    /// Check if a type implements Future
    pub fn has_future_impl(&self, type_name: &str) -> bool {
        self.future_impls.contains_key(type_name)
    }

    /// Get the output type of a Future implementation
    pub fn get_output_type(&self, type_name: &str) -> Option<&Type> {
        self.future_impls
            .get(type_name)
            .map(|impl_info| &impl_info.output_type)
    }

    /// Validate Future trait implementation
    pub fn validate_future_impl(&self, type_name: &str) -> CompilerResult<()> {
        if !self.has_future_impl(type_name) {
            return Err(CompilerError::internal_error(format!(
                "Type '{type_name}' does not implement Future"
            )));
        }
        Ok(())
    }
}

impl Default for FutureTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Pin type for self-referential futures
#[derive(Debug, Clone)]
pub struct Pin<T> {
    pointer: T,
}

impl<T> Pin<T> {
    /// Create a new pinned pointer wrapping the given value.
    pub fn new(pointer: T) -> Self {
        Self { pointer }
    }

    /// Get a shared reference to the pinned value.
    pub fn get_ref(&self) -> &T {
        &self.pointer
    }
}

/// Waker for task waking
#[derive(Debug, Clone)]
pub struct Waker {
    task_id: usize,
}

impl Waker {
    /// Create a new waker for the given task ID.
    pub fn new(task_id: usize) -> Self {
        Self { task_id }
    }

    /// Signal the runtime to re-poll the associated task.
    pub fn wake(&self) {
        // Wake the task (implementation in runtime)
    }

    /// Return the task ID this waker is associated with.
    pub fn task_id(&self) -> usize {
        self.task_id
    }
}

/// Context for polling futures
#[derive(Debug)]
pub struct Context {
    waker: Waker,
}

impl Context {
    /// Create a new polling context with the given waker.
    pub fn new(waker: Waker) -> Self {
        Self { waker }
    }

    /// Get a reference to the waker for this context.
    pub fn waker(&self) -> &Waker {
        &self.waker
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default() {
        let t = FutureTracker::default();
        assert!(!t.has_future_impl("X"));
    }

    #[test]
    fn test_new() {
        let t = FutureTracker::new();
        assert!(t.future_impls.is_empty());
    }

    #[test]
    fn test_register_future_impl() {
        let mut t = FutureTracker::new();
        t.register_future_impl("MyFut".into(), Type::Int);
        assert!(t.has_future_impl("MyFut"));
        assert!(!t.has_future_impl("Other"));
    }

    #[test]
    fn test_get_output_type() {
        let mut t = FutureTracker::new();
        t.register_future_impl("F".into(), Type::String);
        assert_eq!(t.get_output_type("F"), Some(&Type::String));
    }

    #[test]
    fn test_get_output_type_missing() {
        let t = FutureTracker::new();
        assert_eq!(t.get_output_type("Missing"), None);
    }

    #[test]
    fn test_validate_future_impl_ok() {
        let mut t = FutureTracker::new();
        t.register_future_impl("F".into(), Type::Bool);
        assert!(t.validate_future_impl("F").is_ok());
    }

    #[test]
    fn test_validate_future_impl_err() {
        let t = FutureTracker::new();
        assert!(t.validate_future_impl("Missing").is_err());
    }

    #[test]
    fn test_multiple_future_impls() {
        let mut t = FutureTracker::new();
        t.register_future_impl("A".into(), Type::Int);
        t.register_future_impl("B".into(), Type::Float);
        assert!(t.has_future_impl("A"));
        assert!(t.has_future_impl("B"));
        assert_eq!(t.get_output_type("A"), Some(&Type::Int));
        assert_eq!(t.get_output_type("B"), Some(&Type::Float));
    }

    #[test]
    fn test_overwrite_future_impl() {
        let mut t = FutureTracker::new();
        t.register_future_impl("F".into(), Type::Int);
        t.register_future_impl("F".into(), Type::Bool);
        assert_eq!(t.get_output_type("F"), Some(&Type::Bool));
    }

    // --- PollResult ---

    #[test]
    fn test_poll_result_eq() {
        assert_eq!(PollResult::Ready, PollResult::Ready);
        assert_eq!(PollResult::Pending, PollResult::Pending);
        assert_ne!(PollResult::Ready, PollResult::Pending);
    }

    #[test]
    fn test_poll_result_clone() {
        let p = PollResult::Ready;
        let p2 = p.clone();
        assert_eq!(p, p2);
    }

    #[test]
    fn test_poll_result_debug() {
        assert_eq!(format!("{:?}", PollResult::Ready), "Ready");
        assert_eq!(format!("{:?}", PollResult::Pending), "Pending");
    }

    // --- Pin ---

    #[test]
    fn test_pin_new_and_get_ref() {
        let pin = Pin::new(42);
        assert_eq!(*pin.get_ref(), 42);
    }

    #[test]
    fn test_pin_with_string() {
        let pin = Pin::new("hello".to_string());
        assert_eq!(pin.get_ref(), "hello");
    }

    #[test]
    fn test_pin_clone() {
        let pin = Pin::new(99);
        let pin2 = pin.clone();
        assert_eq!(*pin2.get_ref(), 99);
    }

    #[test]
    fn test_pin_debug() {
        let pin = Pin::new(7);
        let s = format!("{:?}", pin);
        assert!(s.contains("7"));
    }

    // --- Waker ---

    #[test]
    fn test_waker_new_and_task_id() {
        let w = Waker::new(123);
        assert_eq!(w.task_id(), 123);
    }

    #[test]
    fn test_waker_wake() {
        let w = Waker::new(0);
        w.wake(); // Should not panic
    }

    #[test]
    fn test_waker_clone() {
        let w = Waker::new(42);
        let w2 = w.clone();
        assert_eq!(w2.task_id(), 42);
    }

    #[test]
    fn test_waker_debug() {
        let w = Waker::new(5);
        let s = format!("{:?}", w);
        assert!(s.contains("5"));
    }

    // --- Context ---

    #[test]
    fn test_context_new_and_waker() {
        let w = Waker::new(456);
        let ctx = Context::new(w);
        assert_eq!(ctx.waker().task_id(), 456);
    }

    #[test]
    fn test_context_debug() {
        let ctx = Context::new(Waker::new(1));
        let s = format!("{:?}", ctx);
        assert!(s.contains("Context"));
    }
}
