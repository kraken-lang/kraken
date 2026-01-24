//! Atomic operations and memory ordering for lock-free programming.

#![allow(dead_code)]

use std::sync::atomic::{
    AtomicBool, AtomicI32, AtomicI64, AtomicIsize, AtomicU32, AtomicU64, AtomicUsize,
    Ordering as StdOrdering,
};

/// Memory ordering for atomic operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ordering {
    Relaxed,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
}

impl Ordering {
    fn to_std(self) -> StdOrdering {
        match self {
            Ordering::Relaxed => StdOrdering::Relaxed,
            Ordering::Acquire => StdOrdering::Acquire,
            Ordering::Release => StdOrdering::Release,
            Ordering::AcqRel => StdOrdering::AcqRel,
            Ordering::SeqCst => StdOrdering::SeqCst,
        }
    }
}

/// Atomic boolean
pub struct AtomicBoolWrapper {
    inner: AtomicBool,
}

impl AtomicBoolWrapper {
    pub fn new(value: bool) -> Self {
        Self {
            inner: AtomicBool::new(value),
        }
    }

    pub fn load(&self, order: Ordering) -> bool {
        self.inner.load(order.to_std())
    }

    pub fn store(&self, value: bool, order: Ordering) {
        self.inner.store(value, order.to_std());
    }

    pub fn swap(&self, value: bool, order: Ordering) -> bool {
        self.inner.swap(value, order.to_std())
    }

    pub fn compare_exchange(
        &self,
        current: bool,
        new: bool,
        success: Ordering,
        failure: Ordering,
    ) -> Result<bool, bool> {
        self.inner
            .compare_exchange(current, new, success.to_std(), failure.to_std())
    }

    pub fn fetch_and(&self, value: bool, order: Ordering) -> bool {
        self.inner.fetch_and(value, order.to_std())
    }

    pub fn fetch_or(&self, value: bool, order: Ordering) -> bool {
        self.inner.fetch_or(value, order.to_std())
    }
}

/// Atomic 32-bit integer
pub struct AtomicI32Wrapper {
    inner: AtomicI32,
}

impl AtomicI32Wrapper {
    pub fn new(value: i32) -> Self {
        Self {
            inner: AtomicI32::new(value),
        }
    }

    pub fn load(&self, order: Ordering) -> i32 {
        self.inner.load(order.to_std())
    }

    pub fn store(&self, value: i32, order: Ordering) {
        self.inner.store(value, order.to_std());
    }

    pub fn swap(&self, value: i32, order: Ordering) -> i32 {
        self.inner.swap(value, order.to_std())
    }

    pub fn compare_exchange(
        &self,
        current: i32,
        new: i32,
        success: Ordering,
        failure: Ordering,
    ) -> Result<i32, i32> {
        self.inner
            .compare_exchange(current, new, success.to_std(), failure.to_std())
    }

    pub fn fetch_add(&self, value: i32, order: Ordering) -> i32 {
        self.inner.fetch_add(value, order.to_std())
    }

    pub fn fetch_sub(&self, value: i32, order: Ordering) -> i32 {
        self.inner.fetch_sub(value, order.to_std())
    }

    pub fn fetch_and(&self, value: i32, order: Ordering) -> i32 {
        self.inner.fetch_and(value, order.to_std())
    }

    pub fn fetch_or(&self, value: i32, order: Ordering) -> i32 {
        self.inner.fetch_or(value, order.to_std())
    }

    pub fn fetch_xor(&self, value: i32, order: Ordering) -> i32 {
        self.inner.fetch_xor(value, order.to_std())
    }
}

/// Atomic 64-bit integer
pub struct AtomicI64Wrapper {
    inner: AtomicI64,
}

impl AtomicI64Wrapper {
    pub fn new(value: i64) -> Self {
        Self {
            inner: AtomicI64::new(value),
        }
    }

    pub fn load(&self, order: Ordering) -> i64 {
        self.inner.load(order.to_std())
    }

    pub fn store(&self, value: i64, order: Ordering) {
        self.inner.store(value, order.to_std());
    }

    pub fn swap(&self, value: i64, order: Ordering) -> i64 {
        self.inner.swap(value, order.to_std())
    }

    pub fn compare_exchange(
        &self,
        current: i64,
        new: i64,
        success: Ordering,
        failure: Ordering,
    ) -> Result<i64, i64> {
        self.inner
            .compare_exchange(current, new, success.to_std(), failure.to_std())
    }

    pub fn fetch_add(&self, value: i64, order: Ordering) -> i64 {
        self.inner.fetch_add(value, order.to_std())
    }

    pub fn fetch_sub(&self, value: i64, order: Ordering) -> i64 {
        self.inner.fetch_sub(value, order.to_std())
    }
}

/// Atomic unsigned 32-bit integer
pub struct AtomicU32Wrapper {
    inner: AtomicU32,
}

impl AtomicU32Wrapper {
    pub fn new(value: u32) -> Self {
        Self {
            inner: AtomicU32::new(value),
        }
    }

    pub fn load(&self, order: Ordering) -> u32 {
        self.inner.load(order.to_std())
    }

    pub fn store(&self, value: u32, order: Ordering) {
        self.inner.store(value, order.to_std());
    }

    pub fn fetch_add(&self, value: u32, order: Ordering) -> u32 {
        self.inner.fetch_add(value, order.to_std())
    }

    pub fn fetch_sub(&self, value: u32, order: Ordering) -> u32 {
        self.inner.fetch_sub(value, order.to_std())
    }
}

/// Atomic unsigned 64-bit integer
pub struct AtomicU64Wrapper {
    inner: AtomicU64,
}

impl AtomicU64Wrapper {
    pub fn new(value: u64) -> Self {
        Self {
            inner: AtomicU64::new(value),
        }
    }

    pub fn load(&self, order: Ordering) -> u64 {
        self.inner.load(order.to_std())
    }

    pub fn store(&self, value: u64, order: Ordering) {
        self.inner.store(value, order.to_std());
    }

    pub fn fetch_add(&self, value: u64, order: Ordering) -> u64 {
        self.inner.fetch_add(value, order.to_std())
    }

    pub fn fetch_sub(&self, value: u64, order: Ordering) -> u64 {
        self.inner.fetch_sub(value, order.to_std())
    }
}

/// Atomic pointer-sized integer
pub struct AtomicUsizeWrapper {
    inner: AtomicUsize,
}

impl AtomicUsizeWrapper {
    pub fn new(value: usize) -> Self {
        Self {
            inner: AtomicUsize::new(value),
        }
    }

    pub fn load(&self, order: Ordering) -> usize {
        self.inner.load(order.to_std())
    }

    pub fn store(&self, value: usize, order: Ordering) {
        self.inner.store(value, order.to_std());
    }

    pub fn fetch_add(&self, value: usize, order: Ordering) -> usize {
        self.inner.fetch_add(value, order.to_std())
    }

    pub fn fetch_sub(&self, value: usize, order: Ordering) -> usize {
        self.inner.fetch_sub(value, order.to_std())
    }

    pub fn compare_exchange(
        &self,
        current: usize,
        new: usize,
        success: Ordering,
        failure: Ordering,
    ) -> Result<usize, usize> {
        self.inner
            .compare_exchange(current, new, success.to_std(), failure.to_std())
    }
}

/// Atomic signed pointer-sized integer
pub struct AtomicIsizeWrapper {
    inner: AtomicIsize,
}

impl AtomicIsizeWrapper {
    pub fn new(value: isize) -> Self {
        Self {
            inner: AtomicIsize::new(value),
        }
    }

    pub fn load(&self, order: Ordering) -> isize {
        self.inner.load(order.to_std())
    }

    pub fn store(&self, value: isize, order: Ordering) {
        self.inner.store(value, order.to_std());
    }

    pub fn fetch_add(&self, value: isize, order: Ordering) -> isize {
        self.inner.fetch_add(value, order.to_std())
    }

    pub fn fetch_sub(&self, value: isize, order: Ordering) -> isize {
        self.inner.fetch_sub(value, order.to_std())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atomic_bool() {
        let atomic = AtomicBoolWrapper::new(false);
        assert!(!atomic.load(Ordering::SeqCst));

        atomic.store(true, Ordering::SeqCst);
        assert!(atomic.load(Ordering::SeqCst));

        let old = atomic.swap(false, Ordering::SeqCst);
        assert!(old);
        assert!(!atomic.load(Ordering::SeqCst));
    }

    #[test]
    fn test_atomic_i32() {
        let atomic = AtomicI32Wrapper::new(0);
        assert_eq!(atomic.load(Ordering::SeqCst), 0);

        atomic.store(42, Ordering::SeqCst);
        assert_eq!(atomic.load(Ordering::SeqCst), 42);

        let old = atomic.fetch_add(10, Ordering::SeqCst);
        assert_eq!(old, 42);
        assert_eq!(atomic.load(Ordering::SeqCst), 52);
    }

    #[test]
    fn test_atomic_i64() {
        let atomic = AtomicI64Wrapper::new(0);
        atomic.store(100, Ordering::SeqCst);
        assert_eq!(atomic.load(Ordering::SeqCst), 100);

        atomic.fetch_sub(50, Ordering::SeqCst);
        assert_eq!(atomic.load(Ordering::SeqCst), 50);
    }

    #[test]
    fn test_compare_exchange() {
        let atomic = AtomicI32Wrapper::new(10);

        let result = atomic.compare_exchange(10, 20, Ordering::SeqCst, Ordering::SeqCst);
        assert_eq!(result, Ok(10));
        assert_eq!(atomic.load(Ordering::SeqCst), 20);

        let result = atomic.compare_exchange(10, 30, Ordering::SeqCst, Ordering::SeqCst);
        assert_eq!(result, Err(20));
        assert_eq!(atomic.load(Ordering::SeqCst), 20);
    }

    #[test]
    fn test_atomic_usize() {
        let atomic = AtomicUsizeWrapper::new(0);
        atomic.fetch_add(5, Ordering::SeqCst);
        assert_eq!(atomic.load(Ordering::SeqCst), 5);

        atomic.fetch_sub(2, Ordering::SeqCst);
        assert_eq!(atomic.load(Ordering::SeqCst), 3);
    }
}
