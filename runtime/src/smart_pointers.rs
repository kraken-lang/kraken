//! Smart pointer runtime implementation for Box, Rc, Arc, and Weak.

#![allow(dead_code)]

use std::alloc::{alloc, dealloc, Layout};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Box<T> runtime - heap allocation with ownership
pub struct BoxRuntime<T> {
    ptr: NonNull<T>,
}

impl<T> BoxRuntime<T> {
    /// Create a new Box with a value
    pub fn new(value: T) -> Self {
        unsafe {
            let layout = Layout::new::<T>();
            let ptr = alloc(layout) as *mut T;
            if ptr.is_null() {
                panic!("Failed to allocate memory for Box");
            }
            ptr.write(value);
            Self {
                ptr: NonNull::new_unchecked(ptr),
            }
        }
    }

    /// Get a reference to the value
    pub fn get(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }

    /// Get a mutable reference to the value
    pub fn get_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T> Drop for BoxRuntime<T> {
    fn drop(&mut self) {
        unsafe {
            let layout = Layout::new::<T>();
            std::ptr::drop_in_place(self.ptr.as_ptr());
            dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}

/// Rc<T> runtime - reference counting for shared ownership
pub struct RcRuntime<T> {
    ptr: NonNull<RcBox<T>>,
}

struct RcBox<T> {
    strong_count: usize,
    weak_count: usize,
    value: T,
}

impl<T> RcRuntime<T> {
    /// Create a new Rc with a value
    pub fn new(value: T) -> Self {
        unsafe {
            let layout = Layout::new::<RcBox<T>>();
            let ptr = alloc(layout) as *mut RcBox<T>;
            if ptr.is_null() {
                panic!("Failed to allocate memory for Rc");
            }
            ptr.write(RcBox {
                strong_count: 1,
                weak_count: 0,
                value,
            });
            Self {
                ptr: NonNull::new_unchecked(ptr),
            }
        }
    }

    /// Clone the Rc (increment reference count)
    pub fn clone_rc(&self) -> Self {
        unsafe {
            (*self.ptr.as_ptr()).strong_count += 1;
            Self { ptr: self.ptr }
        }
    }

    /// Get the strong reference count
    pub fn strong_count(&self) -> usize {
        unsafe { (*self.ptr.as_ptr()).strong_count }
    }

    /// Get a reference to the value
    pub fn get(&self) -> &T {
        unsafe { &(*self.ptr.as_ptr()).value }
    }
}

impl<T> Drop for RcRuntime<T> {
    fn drop(&mut self) {
        unsafe {
            let rc_box = self.ptr.as_mut();
            rc_box.strong_count -= 1;

            if rc_box.strong_count == 0 {
                std::ptr::drop_in_place(&mut rc_box.value);

                if rc_box.weak_count == 0 {
                    let layout = Layout::new::<RcBox<T>>();
                    dealloc(self.ptr.as_ptr() as *mut u8, layout);
                }
            }
        }
    }
}

/// Arc<T> runtime - atomic reference counting for thread safety
pub struct ArcRuntime<T> {
    ptr: NonNull<ArcBox<T>>,
}

struct ArcBox<T> {
    strong_count: AtomicUsize,
    weak_count: AtomicUsize,
    value: T,
}

impl<T> ArcRuntime<T> {
    /// Create a new Arc with a value
    pub fn new(value: T) -> Self {
        unsafe {
            let layout = Layout::new::<ArcBox<T>>();
            let ptr = alloc(layout) as *mut ArcBox<T>;
            if ptr.is_null() {
                panic!("Failed to allocate memory for Arc");
            }
            ptr.write(ArcBox {
                strong_count: AtomicUsize::new(1),
                weak_count: AtomicUsize::new(0),
                value,
            });
            Self {
                ptr: NonNull::new_unchecked(ptr),
            }
        }
    }

    /// Clone the Arc (atomic increment reference count)
    pub fn clone_arc(&self) -> Self {
        unsafe {
            (*self.ptr.as_ptr())
                .strong_count
                .fetch_add(1, Ordering::Relaxed);
            Self { ptr: self.ptr }
        }
    }

    /// Get the strong reference count
    pub fn strong_count(&self) -> usize {
        unsafe { (*self.ptr.as_ptr()).strong_count.load(Ordering::Relaxed) }
    }

    /// Get a reference to the value
    pub fn get(&self) -> &T {
        unsafe { &(*self.ptr.as_ptr()).value }
    }
}

impl<T> Drop for ArcRuntime<T> {
    fn drop(&mut self) {
        unsafe {
            let arc_box = &*self.ptr.as_ptr();

            if arc_box.strong_count.fetch_sub(1, Ordering::Release) == 1 {
                std::sync::atomic::fence(Ordering::Acquire);
                std::ptr::drop_in_place(&mut (*self.ptr.as_ptr()).value);

                if arc_box.weak_count.load(Ordering::Relaxed) == 0 {
                    let layout = Layout::new::<ArcBox<T>>();
                    dealloc(self.ptr.as_ptr() as *mut u8, layout);
                }
            }
        }
    }
}

unsafe impl<T: Send> Send for ArcRuntime<T> {}
unsafe impl<T: Sync> Sync for ArcRuntime<T> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_box_creation() {
        let b = BoxRuntime::new(42);
        assert_eq!(*b.get(), 42);
    }

    #[test]
    fn test_box_mut() {
        let mut b = BoxRuntime::new(42);
        *b.get_mut() = 100;
        assert_eq!(*b.get(), 100);
    }

    #[test]
    fn test_rc_creation() {
        let rc = RcRuntime::new(42);
        assert_eq!(*rc.get(), 42);
        assert_eq!(rc.strong_count(), 1);
    }

    #[test]
    fn test_rc_clone() {
        let rc1 = RcRuntime::new(42);
        let rc2 = rc1.clone_rc();
        assert_eq!(rc1.strong_count(), 2);
        assert_eq!(rc2.strong_count(), 2);
        assert_eq!(*rc1.get(), 42);
        assert_eq!(*rc2.get(), 42);
    }

    #[test]
    fn test_arc_creation() {
        let arc = ArcRuntime::new(42);
        assert_eq!(*arc.get(), 42);
        assert_eq!(arc.strong_count(), 1);
    }

    #[test]
    fn test_arc_clone() {
        let arc1 = ArcRuntime::new(42);
        let arc2 = arc1.clone_arc();
        assert_eq!(arc1.strong_count(), 2);
        assert_eq!(arc2.strong_count(), 2);
        assert_eq!(*arc1.get(), 42);
        assert_eq!(*arc2.get(), 42);
    }

    #[test]
    fn test_arc_thread_safety() {
        let arc = ArcRuntime::new(42);
        let arc_clone = arc.clone_arc();

        let handle = std::thread::spawn(move || {
            assert_eq!(*arc_clone.get(), 42);
        });

        handle.join().unwrap();
        assert_eq!(*arc.get(), 42);
    }
}
