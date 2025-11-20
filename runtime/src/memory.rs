use std::alloc::{alloc, dealloc, Layout};
use std::ptr::NonNull;

/// Memory allocation result type.
pub type MemoryResult<T> = Result<T, MemoryError>;

/// Memory management errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryError {
    /// Allocation failed due to insufficient memory
    AllocationFailed,
    /// Invalid memory layout
    InvalidLayout,
    /// Null pointer dereference
    NullPointer,
    /// Memory already freed
    DoubleFree,
}

impl std::fmt::Display for MemoryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AllocationFailed => write!(f, "Memory allocation failed"),
            Self::InvalidLayout => write!(f, "Invalid memory layout"),
            Self::NullPointer => write!(f, "Null pointer dereference"),
            Self::DoubleFree => write!(f, "Attempted to free already freed memory"),
        }
    }
}

impl std::error::Error for MemoryError {}

/// Manual memory allocator for Kraken runtime.
/// 
/// Provides low-level memory allocation primitives with optional
/// tracking for development mode garbage collection warnings.
pub struct Allocator {
    /// Track allocations in dev mode
    track_allocations: bool,
    /// Total bytes allocated
    total_allocated: std::sync::atomic::AtomicUsize,
    /// Total bytes freed
    total_freed: std::sync::atomic::AtomicUsize,
}

impl Allocator {
    /// Create a new allocator.
    /// 
    /// # Arguments
    /// * `track_allocations` - Enable allocation tracking for GC warnings
    pub fn new(track_allocations: bool) -> Self {
        Self {
            track_allocations,
            total_allocated: std::sync::atomic::AtomicUsize::new(0),
            total_freed: std::sync::atomic::AtomicUsize::new(0),
        }
    }

    /// Allocate memory for a value of type T.
    /// 
    /// # Returns
    /// A non-null pointer to the allocated memory
    /// 
    /// # Errors
    /// Returns `MemoryError::AllocationFailed` if allocation fails
    /// Returns `MemoryError::InvalidLayout` if the layout is invalid
    pub fn allocate<T>(&self) -> MemoryResult<NonNull<T>> {
        let layout = Layout::new::<T>();
        
        if layout.size() == 0 {
            return Err(MemoryError::InvalidLayout);
        }

        let ptr = unsafe { alloc(layout) as *mut T };
        
        if ptr.is_null() {
            return Err(MemoryError::AllocationFailed);
        }

        if self.track_allocations {
            self.total_allocated.fetch_add(
                layout.size(),
                std::sync::atomic::Ordering::Relaxed,
            );
        }

        Ok(unsafe { NonNull::new_unchecked(ptr) })
    }

    /// Allocate memory for an array of values.
    /// 
    /// # Arguments
    /// * `count` - Number of elements to allocate
    /// 
    /// # Returns
    /// A non-null pointer to the allocated memory
    /// 
    /// # Errors
    /// Returns `MemoryError::AllocationFailed` if allocation fails
    /// Returns `MemoryError::InvalidLayout` if the layout is invalid
    pub fn allocate_array<T>(&self, count: usize) -> MemoryResult<NonNull<T>> {
        if count == 0 {
            return Err(MemoryError::InvalidLayout);
        }

        let layout = Layout::array::<T>(count)
            .map_err(|_| MemoryError::InvalidLayout)?;

        let ptr = unsafe { alloc(layout) as *mut T };
        
        if ptr.is_null() {
            return Err(MemoryError::AllocationFailed);
        }

        if self.track_allocations {
            self.total_allocated.fetch_add(
                layout.size(),
                std::sync::atomic::Ordering::Relaxed,
            );
        }

        Ok(unsafe { NonNull::new_unchecked(ptr) })
    }

    /// Deallocate memory for a value.
    /// 
    /// # Arguments
    /// * `ptr` - Pointer to the memory to deallocate
    /// 
    /// # Safety
    /// The pointer must have been allocated by this allocator and not yet freed.
    pub unsafe fn deallocate<T>(&self, ptr: NonNull<T>) {
        let layout = Layout::new::<T>();
        
        if self.track_allocations {
            self.total_freed.fetch_add(
                layout.size(),
                std::sync::atomic::Ordering::Relaxed,
            );
        }

        dealloc(ptr.as_ptr() as *mut u8, layout);
    }

    /// Deallocate memory for an array.
    /// 
    /// # Arguments
    /// * `ptr` - Pointer to the array memory to deallocate
    /// * `count` - Number of elements in the array
    /// 
    /// # Safety
    /// The pointer must have been allocated by this allocator and not yet freed.
    /// The count must match the original allocation count.
    pub unsafe fn deallocate_array<T>(&self, ptr: NonNull<T>, count: usize) {
        if let Ok(layout) = Layout::array::<T>(count) {
            if self.track_allocations {
                self.total_freed.fetch_add(
                    layout.size(),
                    std::sync::atomic::Ordering::Relaxed,
                );
            }

            dealloc(ptr.as_ptr() as *mut u8, layout);
        }
    }

    /// Get total bytes allocated.
    pub fn total_allocated(&self) -> usize {
        self.total_allocated.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Get total bytes freed.
    pub fn total_freed(&self) -> usize {
        self.total_freed.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Get current memory usage (allocated - freed).
    pub fn current_usage(&self) -> usize {
        self.total_allocated() - self.total_freed()
    }

    /// Check if allocations are being tracked.
    pub fn is_tracking(&self) -> bool {
        self.track_allocations
    }
}

impl Default for Allocator {
    fn default() -> Self {
        Self::new(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocate_and_deallocate() {
        let allocator = Allocator::new(true);
        
        let ptr = allocator.allocate::<i32>().expect("allocation failed");
        assert_eq!(allocator.current_usage(), std::mem::size_of::<i32>());
        
        unsafe {
            allocator.deallocate(ptr);
        }
        assert_eq!(allocator.current_usage(), 0);
    }

    #[test]
    fn test_allocate_array() {
        let allocator = Allocator::new(true);
        
        let count = 10;
        let ptr = allocator.allocate_array::<i32>(count).expect("allocation failed");
        assert_eq!(allocator.current_usage(), std::mem::size_of::<i32>() * count);
        
        unsafe {
            allocator.deallocate_array(ptr, count);
        }
        assert_eq!(allocator.current_usage(), 0);
    }

    #[test]
    fn test_zero_size_allocation() {
        let allocator = Allocator::new(false);
        let result = allocator.allocate_array::<i32>(0);
        assert!(result.is_err());
    }

    #[test]
    fn test_tracking_disabled() {
        let allocator = Allocator::new(false);
        assert!(!allocator.is_tracking());
        
        let ptr = allocator.allocate::<i32>().expect("allocation failed");
        assert_eq!(allocator.current_usage(), 0);
        
        unsafe {
            allocator.deallocate(ptr);
        }
    }
}
