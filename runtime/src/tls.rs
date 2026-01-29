//! Thread-local storage support for Kraken runtime.
//!
//! Provides thread-local storage primitives for storing per-thread data.

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Thread-local storage key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TlsKey(usize);

static NEXT_KEY: AtomicUsize = AtomicUsize::new(0);

impl TlsKey {
    /// Create a new TLS key.
    pub fn new() -> Self {
        Self(NEXT_KEY.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for TlsKey {
    fn default() -> Self {
        Self::new()
    }
}

thread_local! {
    static TLS_STORAGE: RefCell<HashMap<usize, Box<dyn std::any::Any>>> = RefCell::new(HashMap::new());
}

/// Set a value in thread-local storage.
pub fn tls_set<T: 'static>(key: TlsKey, value: T) {
    TLS_STORAGE.with(|storage| {
        storage.borrow_mut().insert(key.0, Box::new(value));
    });
}

/// Get a value from thread-local storage.
pub fn tls_get<T: 'static + Clone>(key: TlsKey) -> Option<T> {
    TLS_STORAGE.with(|storage| {
        storage
            .borrow()
            .get(&key.0)
            .and_then(|boxed| boxed.downcast_ref::<T>())
            .cloned()
    })
}

/// Remove a value from thread-local storage.
pub fn tls_remove(key: TlsKey) {
    TLS_STORAGE.with(|storage| {
        storage.borrow_mut().remove(&key.0);
    });
}

/// Check if a key exists in thread-local storage.
pub fn tls_contains(key: TlsKey) -> bool {
    TLS_STORAGE.with(|storage| storage.borrow().contains_key(&key.0))
}

/// Clear all thread-local storage for the current thread.
pub fn tls_clear() {
    TLS_STORAGE.with(|storage| {
        storage.borrow_mut().clear();
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tls_basic() {
        let key = TlsKey::new();
        tls_set(key, 42i32);
        assert_eq!(tls_get::<i32>(key), Some(42));
    }

    #[test]
    fn test_tls_different_types() {
        let key1 = TlsKey::new();
        let key2 = TlsKey::new();

        tls_set(key1, 42i32);
        tls_set(key2, "hello".to_string());

        assert_eq!(tls_get::<i32>(key1), Some(42));
        assert_eq!(tls_get::<String>(key2), Some("hello".to_string()));
    }

    #[test]
    fn test_tls_remove() {
        let key = TlsKey::new();
        tls_set(key, 42i32);
        assert!(tls_contains(key));

        tls_remove(key);
        assert!(!tls_contains(key));
        assert_eq!(tls_get::<i32>(key), None);
    }

    #[test]
    fn test_tls_clear() {
        let key1 = TlsKey::new();
        let key2 = TlsKey::new();

        tls_set(key1, 42i32);
        tls_set(key2, "hello".to_string());

        tls_clear();

        assert!(!tls_contains(key1));
        assert!(!tls_contains(key2));
    }

    #[test]
    fn test_tls_thread_isolation() {
        use std::thread;

        let key = TlsKey::new();
        tls_set(key, 42i32);

        let handle = thread::spawn(move || {
            // Different thread should not see the value
            assert_eq!(tls_get::<i32>(key), None);
            tls_set(key, 100i32);
            assert_eq!(tls_get::<i32>(key), Some(100));
        });

        handle.join().unwrap();

        // Original thread should still have its value
        assert_eq!(tls_get::<i32>(key), Some(42));
    }
}
