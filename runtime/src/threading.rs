//! Threading primitives for concurrent programming.

#![allow(dead_code)]

use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle, ThreadId};
use std::time::Duration;

/// Thread builder for configuring threads
pub struct ThreadBuilder {
    name: Option<String>,
    stack_size: Option<usize>,
}

impl ThreadBuilder {
    pub fn new() -> Self {
        Self {
            name: None,
            stack_size: None,
        }
    }

    pub fn name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }

    pub fn stack_size(mut self, size: usize) -> Self {
        self.stack_size = Some(size);
        self
    }

    pub fn spawn<F, T>(self, f: F) -> std::io::Result<Thread<T>>
    where
        F: FnOnce() -> T + Send + 'static,
        T: Send + 'static,
    {
        let mut builder = thread::Builder::new();

        if let Some(name) = self.name {
            builder = builder.name(name);
        }

        if let Some(size) = self.stack_size {
            builder = builder.stack_size(size);
        }

        let handle = builder.spawn(f)?;
        Ok(Thread { handle })
    }
}

impl Default for ThreadBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Thread handle
pub struct Thread<T> {
    handle: JoinHandle<T>,
}

impl<T> Thread<T> {
    pub fn join(self) -> Result<T, Box<dyn std::any::Any + Send>> {
        self.handle.join()
    }

    pub fn thread(&self) -> &thread::Thread {
        self.handle.thread()
    }
}

/// Spawn a new thread
pub fn spawn<F, T>(f: F) -> Thread<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    Thread {
        handle: thread::spawn(f),
    }
}

/// Get the current thread ID
pub fn current_thread_id() -> ThreadId {
    thread::current().id()
}

/// Get the current thread name
pub fn current_thread_name() -> Option<String> {
    thread::current().name().map(|s| s.to_string())
}

/// Sleep for a duration
pub fn sleep(duration: Duration) {
    thread::sleep(duration);
}

/// Yield the current thread
pub fn yield_now() {
    thread::yield_now();
}

/// Thread-local storage
pub struct ThreadLocal<T> {
    inner: Arc<Mutex<std::collections::HashMap<ThreadId, T>>>,
}

impl<T: Clone> ThreadLocal<T> {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(std::collections::HashMap::new())),
        }
    }

    pub fn get(&self) -> Option<T> {
        let map = self.inner.lock().unwrap();
        map.get(&thread::current().id()).cloned()
    }

    pub fn set(&self, value: T) {
        let mut map = self.inner.lock().unwrap();
        map.insert(thread::current().id(), value);
    }

    pub fn remove(&self) {
        let mut map = self.inner.lock().unwrap();
        map.remove(&thread::current().id());
    }
}

impl<T: Clone> Clone for ThreadLocal<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T: Clone> Default for ThreadLocal<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_spawn_thread() {
        let handle = spawn(|| 42);
        let result = handle.join().unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_thread_builder() {
        let handle = ThreadBuilder::new()
            .name("test-thread".to_string())
            .spawn(|| 100)
            .unwrap();

        let result = handle.join().unwrap();
        assert_eq!(result, 100);
    }

    #[test]
    fn test_current_thread_id() {
        let id1 = current_thread_id();
        let id2 = current_thread_id();
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_thread_local() {
        let tls = ThreadLocal::new();
        tls.set(42);
        assert_eq!(tls.get(), Some(42));

        let tls_clone = tls.clone();
        let handle = spawn(move || {
            tls_clone.set(100);
            tls_clone.get()
        });

        let result = handle.join().unwrap();
        assert_eq!(result, Some(100));
        assert_eq!(tls.get(), Some(42)); // Original thread still has 42
    }

    #[test]
    fn test_thread_local_remove() {
        let tls = ThreadLocal::new();
        tls.set(42);
        assert_eq!(tls.get(), Some(42));
        tls.remove();
        assert_eq!(tls.get(), None);
    }

    #[test]
    fn test_multiple_threads() {
        let handles: Vec<_> = (0..5).map(|i| spawn(move || i * 2)).collect();

        let results: Vec<_> = handles.into_iter().map(|h| h.join().unwrap()).collect();

        assert_eq!(results, vec![0, 2, 4, 6, 8]);
    }
}
