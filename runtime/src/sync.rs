//! Synchronization primitives for concurrent programming.

#![allow(dead_code)]

use std::sync::{Arc, Condvar, Mutex as StdMutex, RwLock as StdRwLock};
use std::time::Duration;

/// Mutex for mutual exclusion
pub struct Mutex<T> {
    inner: Arc<StdMutex<T>>,
}

impl<T> Mutex<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Arc::new(StdMutex::new(value)),
        }
    }

    pub fn lock(&self) -> std::sync::MutexGuard<T> {
        self.inner.lock().unwrap()
    }

    pub fn try_lock(&self) -> Option<std::sync::MutexGuard<T>> {
        self.inner.try_lock().ok()
    }
}

impl<T> Clone for Mutex<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

/// Reader-writer lock
pub struct RwLock<T> {
    inner: Arc<StdRwLock<T>>,
}

impl<T> RwLock<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Arc::new(StdRwLock::new(value)),
        }
    }

    pub fn read(&self) -> std::sync::RwLockReadGuard<T> {
        self.inner.read().unwrap()
    }

    pub fn write(&self) -> std::sync::RwLockWriteGuard<T> {
        self.inner.write().unwrap()
    }

    pub fn try_read(&self) -> Option<std::sync::RwLockReadGuard<T>> {
        self.inner.try_read().ok()
    }

    pub fn try_write(&self) -> Option<std::sync::RwLockWriteGuard<T>> {
        self.inner.try_write().ok()
    }
}

impl<T> Clone for RwLock<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

/// Semaphore for counting
pub struct Semaphore {
    permits: Arc<StdMutex<usize>>,
    condvar: Arc<Condvar>,
}

impl Semaphore {
    pub fn new(permits: usize) -> Self {
        Self {
            permits: Arc::new(StdMutex::new(permits)),
            condvar: Arc::new(Condvar::new()),
        }
    }

    pub fn acquire(&self) {
        let mut permits = self.permits.lock().unwrap();
        while *permits == 0 {
            permits = self.condvar.wait(permits).unwrap();
        }
        *permits -= 1;
    }

    pub fn try_acquire(&self) -> bool {
        let mut permits = self.permits.lock().unwrap();
        if *permits > 0 {
            *permits -= 1;
            true
        } else {
            false
        }
    }

    pub fn release(&self) {
        let mut permits = self.permits.lock().unwrap();
        *permits += 1;
        self.condvar.notify_one();
    }

    pub fn available_permits(&self) -> usize {
        let permits = self.permits.lock().unwrap();
        *permits
    }
}

impl Clone for Semaphore {
    fn clone(&self) -> Self {
        Self {
            permits: self.permits.clone(),
            condvar: self.condvar.clone(),
        }
    }
}

/// Barrier for synchronization points
pub struct Barrier {
    count: Arc<StdMutex<usize>>,
    total: usize,
    condvar: Arc<Condvar>,
}

impl Barrier {
    pub fn new(total: usize) -> Self {
        Self {
            count: Arc::new(StdMutex::new(0)),
            total,
            condvar: Arc::new(Condvar::new()),
        }
    }

    pub fn wait(&self) -> bool {
        let mut count = self.count.lock().unwrap();
        *count += 1;

        if *count >= self.total {
            *count = 0;
            self.condvar.notify_all();
            true // Last one to arrive
        } else {
            while *count > 0 && *count < self.total {
                count = self.condvar.wait(count).unwrap();
            }
            false
        }
    }
}

impl Clone for Barrier {
    fn clone(&self) -> Self {
        Self {
            count: self.count.clone(),
            total: self.total,
            condvar: self.condvar.clone(),
        }
    }
}

/// Condition variable
pub struct ConditionVariable {
    condvar: Arc<Condvar>,
}

impl ConditionVariable {
    pub fn new() -> Self {
        Self {
            condvar: Arc::new(Condvar::new()),
        }
    }

    pub fn wait<'a, T>(&self, guard: std::sync::MutexGuard<'a, T>) -> std::sync::MutexGuard<'a, T> {
        self.condvar.wait(guard).unwrap()
    }

    pub fn wait_timeout<'a, T>(
        &self,
        guard: std::sync::MutexGuard<'a, T>,
        duration: Duration,
    ) -> (std::sync::MutexGuard<'a, T>, bool) {
        let result = self.condvar.wait_timeout(guard, duration).unwrap();
        (result.0, result.1.timed_out())
    }

    pub fn notify_one(&self) {
        self.condvar.notify_one();
    }

    pub fn notify_all(&self) {
        self.condvar.notify_all();
    }
}

impl Clone for ConditionVariable {
    fn clone(&self) -> Self {
        Self {
            condvar: self.condvar.clone(),
        }
    }
}

impl Default for ConditionVariable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_mutex() {
        let mutex = Mutex::new(0);
        {
            let mut guard = mutex.lock();
            *guard = 42;
        }
        let guard = mutex.lock();
        assert_eq!(*guard, 42);
    }

    #[test]
    fn test_mutex_concurrent() {
        let mutex = Mutex::new(0);
        let mutex_clone = mutex.clone();

        let handle = thread::spawn(move || {
            let mut guard = mutex_clone.lock();
            *guard += 1;
        });

        handle.join().unwrap();
        let guard = mutex.lock();
        assert_eq!(*guard, 1);
    }

    #[test]
    fn test_rwlock() {
        let lock = RwLock::new(0);
        {
            let mut writer = lock.write();
            *writer = 42;
        }
        let reader = lock.read();
        assert_eq!(*reader, 42);
    }

    #[test]
    fn test_rwlock_multiple_readers() {
        let lock = RwLock::new(42);
        let lock_clone1 = lock.clone();
        let lock_clone2 = lock.clone();

        let handle1 = thread::spawn(move || {
            let reader = lock_clone1.read();
            *reader
        });

        let handle2 = thread::spawn(move || {
            let reader = lock_clone2.read();
            *reader
        });

        assert_eq!(handle1.join().unwrap(), 42);
        assert_eq!(handle2.join().unwrap(), 42);
    }

    #[test]
    fn test_semaphore() {
        let sem = Semaphore::new(2);
        assert_eq!(sem.available_permits(), 2);

        sem.acquire();
        assert_eq!(sem.available_permits(), 1);

        sem.acquire();
        assert_eq!(sem.available_permits(), 0);

        assert!(!sem.try_acquire());

        sem.release();
        assert_eq!(sem.available_permits(), 1);
    }

    #[test]
    fn test_barrier() {
        let barrier = Barrier::new(3);
        let barrier1 = barrier.clone();
        let barrier2 = barrier.clone();

        let handle1 = thread::spawn(move || barrier1.wait());
        let handle2 = thread::spawn(move || barrier2.wait());

        barrier.wait();

        handle1.join().unwrap();
        handle2.join().unwrap();

        // Test passes if all threads complete without deadlock
    }

    #[test]
    fn test_condition_variable() {
        let mutex = Arc::new(StdMutex::new(false));
        let condvar = ConditionVariable::new();

        let mutex_clone = mutex.clone();
        let condvar_clone = condvar.clone();

        let handle = thread::spawn(move || {
            let mut guard = mutex_clone.lock().unwrap();
            *guard = true;
            condvar_clone.notify_one();
        });

        let mut guard = mutex.lock().unwrap();
        while !*guard {
            guard = condvar.wait(guard);
        }

        handle.join().unwrap();
        assert!(*guard);
    }
}
