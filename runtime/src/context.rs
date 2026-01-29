//! Context propagation utilities for Kraken runtime.
//!
//! Provides utilities for propagating context across async boundaries and threads.

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// Context key for storing typed values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ContextKey(usize);

static NEXT_CONTEXT_KEY: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

impl ContextKey {
    /// Create a new context key.
    pub fn new() -> Self {
        Self(NEXT_CONTEXT_KEY.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

impl Default for ContextKey {
    fn default() -> Self {
        Self::new()
    }
}

/// Context for propagating values across boundaries.
#[derive(Clone)]
pub struct Context {
    values: Arc<RwLock<HashMap<usize, Arc<dyn std::any::Any + Send + Sync>>>>,
}

impl Context {
    /// Create a new empty context.
    pub fn new() -> Self {
        Self {
            values: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Set a value in the context.
    pub fn set<T: Send + Sync + 'static>(&self, key: ContextKey, value: T) {
        let mut values = self.values.write().unwrap();
        values.insert(key.0, Arc::new(value));
    }

    /// Get a value from the context.
    pub fn get<T: Send + Sync + 'static>(&self, key: ContextKey) -> Option<Arc<T>> {
        let values = self.values.read().unwrap();
        values.get(&key.0).and_then(|v| v.clone().downcast().ok())
    }

    /// Remove a value from the context.
    pub fn remove(&self, key: ContextKey) -> bool {
        let mut values = self.values.write().unwrap();
        values.remove(&key.0).is_some()
    }

    /// Check if a key exists in the context.
    pub fn contains(&self, key: ContextKey) -> bool {
        let values = self.values.read().unwrap();
        values.contains_key(&key.0)
    }

    /// Clear all values from the context.
    pub fn clear(&self) {
        let mut values = self.values.write().unwrap();
        values.clear();
    }

    /// Create a child context that inherits from this one.
    pub fn child(&self) -> Self {
        Self {
            values: Arc::new(RwLock::new(self.values.read().unwrap().clone())),
        }
    }

    /// Merge another context into this one.
    pub fn merge(&self, other: &Context) {
        let mut values = self.values.write().unwrap();
        let other_values = other.values.read().unwrap();
        values.extend(other_values.iter().map(|(k, v)| (*k, v.clone())));
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for creating contexts with values.
pub struct ContextBuilder {
    context: Context,
}

impl ContextBuilder {
    /// Create a new context builder.
    pub fn new() -> Self {
        Self {
            context: Context::new(),
        }
    }

    /// Add a value to the context.
    pub fn with<T: Send + Sync + 'static>(self, key: ContextKey, value: T) -> Self {
        self.context.set(key, value);
        self
    }

    /// Build the context.
    pub fn build(self) -> Context {
        self.context
    }
}

impl Default for ContextBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_basic() {
        let ctx = Context::new();
        let key = ContextKey::new();

        ctx.set(key, 42i32);
        let value = ctx.get::<i32>(key);
        assert_eq!(value.map(|v| *v), Some(42));
    }

    #[test]
    fn test_context_different_types() {
        let ctx = Context::new();
        let key1 = ContextKey::new();
        let key2 = ContextKey::new();

        ctx.set(key1, 42i32);
        ctx.set(key2, "hello".to_string());

        assert_eq!(ctx.get::<i32>(key1).map(|v| *v), Some(42));
        assert_eq!(
            ctx.get::<String>(key2)
                .as_ref()
                .map(|v| v.as_ref().as_str()),
            Some("hello")
        );
    }

    #[test]
    fn test_context_remove() {
        let ctx = Context::new();
        let key = ContextKey::new();

        ctx.set(key, 42i32);
        assert!(ctx.contains(key));

        assert!(ctx.remove(key));
        assert!(!ctx.contains(key));
        assert!(ctx.get::<i32>(key).is_none());
    }

    #[test]
    fn test_context_clear() {
        let ctx = Context::new();
        let key1 = ContextKey::new();
        let key2 = ContextKey::new();

        ctx.set(key1, 42i32);
        ctx.set(key2, "hello".to_string());

        ctx.clear();

        assert!(!ctx.contains(key1));
        assert!(!ctx.contains(key2));
    }

    #[test]
    fn test_context_child() {
        let parent = Context::new();
        let key = ContextKey::new();

        parent.set(key, 42i32);

        let child = parent.child();
        assert_eq!(child.get::<i32>(key).map(|v| *v), Some(42));

        // Modify child doesn't affect parent
        let key2 = ContextKey::new();
        child.set(key2, "child".to_string());

        assert!(child.contains(key2));
        assert!(!parent.contains(key2));
    }

    #[test]
    fn test_context_merge() {
        let ctx1 = Context::new();
        let ctx2 = Context::new();

        let key1 = ContextKey::new();
        let key2 = ContextKey::new();

        ctx1.set(key1, 42i32);
        ctx2.set(key2, "hello".to_string());

        ctx1.merge(&ctx2);

        assert_eq!(ctx1.get::<i32>(key1).map(|v| *v), Some(42));
        let s = ctx1.get::<String>(key2);
        assert_eq!(s.as_ref().map(|v| v.as_ref().as_str()), Some("hello"));
    }

    #[test]
    fn test_context_builder() {
        let key1 = ContextKey::new();
        let key2 = ContextKey::new();

        let ctx = ContextBuilder::new()
            .with(key1, 42i32)
            .with(key2, "hello".to_string())
            .build();

        assert_eq!(ctx.get::<i32>(key1).map(|v| *v), Some(42));
        let s = ctx.get::<String>(key2);
        assert_eq!(s.as_ref().map(|v| v.as_ref().as_str()), Some("hello"));
    }

    #[test]
    fn test_context_thread_safety() {
        use std::thread;

        let ctx = Context::new();
        let key = ContextKey::new();
        ctx.set(key, 42i32);

        let ctx_clone = ctx.clone();
        let handle = thread::spawn(move || {
            assert_eq!(ctx_clone.get::<i32>(key).map(|v| *v), Some(42));
        });

        handle.join().unwrap();
    }
}
