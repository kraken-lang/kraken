//! Async primitives for concurrent programming.

#![allow(dead_code)]

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

/// Async channel sender (MPSC - Multiple Producer Single Consumer)
pub struct AsyncSender<T> {
    queue: Arc<Mutex<VecDeque<T>>>,
}

impl<T> AsyncSender<T> {
    /// Send a value through the channel
    pub fn send(&self, value: T) {
        let mut queue = self.queue.lock().unwrap();
        queue.push_back(value);
    }
}

impl<T> Clone for AsyncSender<T> {
    fn clone(&self) -> Self {
        Self {
            queue: self.queue.clone(),
        }
    }
}

/// Async channel receiver
pub struct AsyncReceiver<T> {
    queue: Arc<Mutex<VecDeque<T>>>,
}

impl<T> AsyncReceiver<T> {
    /// Try to receive a value from the channel
    pub fn try_recv(&self) -> Option<T> {
        let mut queue = self.queue.lock().unwrap();
        queue.pop_front()
    }

    /// Check if the channel is empty
    pub fn is_empty(&self) -> bool {
        let queue = self.queue.lock().unwrap();
        queue.is_empty()
    }
}

/// Create an async channel (MPSC)
pub fn async_channel<T>() -> (AsyncSender<T>, AsyncReceiver<T>) {
    let queue = Arc::new(Mutex::new(VecDeque::new()));
    let sender = AsyncSender {
        queue: queue.clone(),
    };
    let receiver = AsyncReceiver { queue };
    (sender, receiver)
}

/// Oneshot channel for single value transmission
pub struct OneshotSender<T> {
    value: Arc<Mutex<Option<T>>>,
}

impl<T> OneshotSender<T> {
    /// Send a value through the oneshot channel
    pub fn send(self, value: T) -> Result<(), T> {
        let mut slot = self.value.lock().unwrap();
        if slot.is_some() {
            return Err(value);
        }
        *slot = Some(value);
        Ok(())
    }
}

/// Oneshot channel receiver
pub struct OneshotReceiver<T> {
    value: Arc<Mutex<Option<T>>>,
}

impl<T> OneshotReceiver<T> {
    /// Try to receive the value
    pub fn try_recv(&self) -> Option<T> {
        let mut slot = self.value.lock().unwrap();
        slot.take()
    }
}

/// Create a oneshot channel
pub fn oneshot<T>() -> (OneshotSender<T>, OneshotReceiver<T>) {
    let value = Arc::new(Mutex::new(None));
    let sender = OneshotSender {
        value: value.clone(),
    };
    let receiver = OneshotReceiver { value };
    (sender, receiver)
}

/// Async mutex for mutual exclusion
pub struct AsyncMutex<T> {
    inner: Arc<Mutex<T>>,
}

impl<T> AsyncMutex<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Arc::new(Mutex::new(value)),
        }
    }

    /// Try to lock the mutex
    pub fn try_lock(&self) -> Option<std::sync::MutexGuard<'_, T>> {
        self.inner.try_lock().ok()
    }

    /// Lock the mutex (blocking)
    pub fn lock(&self) -> std::sync::MutexGuard<'_, T> {
        self.inner.lock().unwrap()
    }
}

impl<T> Clone for AsyncMutex<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

/// Async semaphore for counting
pub struct AsyncSemaphore {
    permits: Arc<Mutex<usize>>,
}

impl AsyncSemaphore {
    pub fn new(permits: usize) -> Self {
        Self {
            permits: Arc::new(Mutex::new(permits)),
        }
    }

    /// Try to acquire a permit
    pub fn try_acquire(&self) -> bool {
        let mut permits = self.permits.lock().unwrap();
        if *permits > 0 {
            *permits -= 1;
            true
        } else {
            false
        }
    }

    /// Release a permit
    pub fn release(&self) {
        let mut permits = self.permits.lock().unwrap();
        *permits += 1;
    }

    /// Get available permits
    pub fn available_permits(&self) -> usize {
        let permits = self.permits.lock().unwrap();
        *permits
    }
}

impl Clone for AsyncSemaphore {
    fn clone(&self) -> Self {
        Self {
            permits: self.permits.clone(),
        }
    }
}

/// Async barrier for synchronization
pub struct AsyncBarrier {
    count: Arc<Mutex<usize>>,
    total: usize,
}

impl AsyncBarrier {
    pub fn new(total: usize) -> Self {
        Self {
            count: Arc::new(Mutex::new(0)),
            total,
        }
    }

    /// Wait at the barrier
    pub fn wait(&self) -> bool {
        let mut count = self.count.lock().unwrap();
        *count += 1;
        if *count >= self.total {
            *count = 0;
            true // Last one to arrive
        } else {
            false
        }
    }

    /// Get current count
    pub fn current_count(&self) -> usize {
        let count = self.count.lock().unwrap();
        *count
    }
}

impl Clone for AsyncBarrier {
    fn clone(&self) -> Self {
        Self {
            count: self.count.clone(),
            total: self.total,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_async_channel() {
        let (sender, receiver) = async_channel();
        sender.send(42);
        sender.send(100);
        assert_eq!(receiver.try_recv(), Some(42));
        assert_eq!(receiver.try_recv(), Some(100));
        assert_eq!(receiver.try_recv(), None);
    }

    #[test]
    fn test_oneshot_channel() {
        let (sender, receiver) = oneshot();
        assert!(sender.send(42).is_ok());
        assert_eq!(receiver.try_recv(), Some(42));
        assert_eq!(receiver.try_recv(), None);
    }

    #[test]
    fn test_async_mutex() {
        let mutex = AsyncMutex::new(0);
        {
            let mut guard = mutex.lock();
            *guard = 42;
        }
        let guard = mutex.lock();
        assert_eq!(*guard, 42);
    }

    #[test]
    fn test_async_semaphore() {
        let sem = AsyncSemaphore::new(2);
        assert_eq!(sem.available_permits(), 2);
        assert!(sem.try_acquire());
        assert_eq!(sem.available_permits(), 1);
        assert!(sem.try_acquire());
        assert_eq!(sem.available_permits(), 0);
        assert!(!sem.try_acquire());
        sem.release();
        assert_eq!(sem.available_permits(), 1);
    }

    #[test]
    fn test_async_barrier() {
        let barrier = AsyncBarrier::new(3);
        assert_eq!(barrier.current_count(), 0);
        assert!(!barrier.wait());
        assert_eq!(barrier.current_count(), 1);
        assert!(!barrier.wait());
        assert_eq!(barrier.current_count(), 2);
        assert!(barrier.wait()); // Last one
        assert_eq!(barrier.current_count(), 0);
    }
}
