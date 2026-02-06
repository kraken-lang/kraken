//! Memory management features including custom allocators and memory pools.

#![allow(dead_code)]

use std::collections::HashMap;

/// Custom allocator trait for memory allocation strategies
pub trait Allocator {
    /// Allocate memory of the specified size
    fn allocate(&mut self, size: usize) -> Option<*mut u8>;
    
    /// Deallocate memory at the specified pointer
    fn deallocate(&mut self, ptr: *mut u8);
    
    /// Get the total allocated memory
    fn total_allocated(&self) -> usize;
}

/// Memory pool allocator for efficient fixed-size allocations
pub struct PoolAllocator {
    block_size: usize,
    pool: Vec<*mut u8>,
    allocated: usize,
}

impl PoolAllocator {
    /// Create a new pool allocator with the given block size and initial capacity.
    pub fn new(block_size: usize, initial_capacity: usize) -> Self {
        Self {
            block_size,
            pool: Vec::with_capacity(initial_capacity),
            allocated: 0,
        }
    }

    /// Return the fixed block size for this pool.
    pub fn block_size(&self) -> usize {
        self.block_size
    }

    /// Return the number of free blocks available in the pool.
    pub fn available_blocks(&self) -> usize {
        self.pool.len()
    }
}

impl Allocator for PoolAllocator {
    fn allocate(&mut self, size: usize) -> Option<*mut u8> {
        if size > self.block_size {
            return None;
        }
        
        if let Some(ptr) = self.pool.pop() {
            self.allocated += self.block_size;
            Some(ptr)
        } else {
            None
        }
    }

    fn deallocate(&mut self, ptr: *mut u8) {
        self.pool.push(ptr);
        self.allocated = self.allocated.saturating_sub(self.block_size);
    }

    fn total_allocated(&self) -> usize {
        self.allocated
    }
}

/// Arena allocator for bump allocation
pub struct ArenaAllocator {
    buffer: Vec<u8>,
    offset: usize,
    allocated: usize,
}

impl ArenaAllocator {
    /// Create a new arena allocator with the given byte capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(capacity),
            offset: 0,
            allocated: 0,
        }
    }

    /// Return the total byte capacity of the arena.
    pub fn capacity(&self) -> usize {
        self.buffer.capacity()
    }

    /// Return the number of bytes currently allocated.
    pub fn used(&self) -> usize {
        self.offset
    }

    /// Reset the arena, freeing all allocations (O(1)).
    pub fn reset(&mut self) {
        self.offset = 0;
        self.allocated = 0;
    }
}

impl Allocator for ArenaAllocator {
    fn allocate(&mut self, size: usize) -> Option<*mut u8> {
        if self.offset + size > self.buffer.capacity() {
            return None;
        }

        let ptr = unsafe { self.buffer.as_mut_ptr().add(self.offset) };
        self.offset += size;
        self.allocated += size;
        Some(ptr)
    }

    fn deallocate(&mut self, _ptr: *mut u8) {
        // Arena allocator doesn't support individual deallocation
    }

    fn total_allocated(&self) -> usize {
        self.allocated
    }
}

/// Allocation profiler for tracking memory usage
pub struct AllocationProfiler {
    allocations: HashMap<String, usize>,
    total_allocated: usize,
    peak_allocated: usize,
}

impl AllocationProfiler {
    /// Create a new allocation profiler with empty tracking state.
    pub fn new() -> Self {
        Self {
            allocations: HashMap::new(),
            total_allocated: 0,
            peak_allocated: 0,
        }
    }

    /// Record an allocation of `size` bytes under the given tag.
    pub fn record_allocation(&mut self, tag: String, size: usize) {
        *self.allocations.entry(tag).or_insert(0) += size;
        self.total_allocated += size;
        if self.total_allocated > self.peak_allocated {
            self.peak_allocated = self.total_allocated;
        }
    }

    /// Record a deallocation of `size` bytes under the given tag.
    pub fn record_deallocation(&mut self, tag: &str, size: usize) {
        if let Some(allocated) = self.allocations.get_mut(tag) {
            *allocated = allocated.saturating_sub(size);
        }
        self.total_allocated = self.total_allocated.saturating_sub(size);
    }

    /// Return the current total bytes allocated.
    pub fn total_allocated(&self) -> usize {
        self.total_allocated
    }

    /// Return the peak total bytes allocated at any point.
    pub fn peak_allocated(&self) -> usize {
        self.peak_allocated
    }

    /// Return a map of tag names to their current allocated byte counts.
    pub fn allocations_by_tag(&self) -> &HashMap<String, usize> {
        &self.allocations
    }
}

impl Default for AllocationProfiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pool_allocator_creation() {
        let pool = PoolAllocator::new(64, 10);
        assert_eq!(pool.block_size(), 64);
        assert_eq!(pool.available_blocks(), 0);
        assert_eq!(pool.total_allocated(), 0);
    }

    #[test]
    fn test_arena_allocator_creation() {
        let arena = ArenaAllocator::new(1024);
        assert_eq!(arena.capacity(), 1024);
        assert_eq!(arena.used(), 0);
        assert_eq!(arena.total_allocated(), 0);
    }

    #[test]
    fn test_arena_reset() {
        let mut arena = ArenaAllocator::new(1024);
        arena.offset = 100;
        arena.allocated = 100;
        arena.reset();
        assert_eq!(arena.used(), 0);
        assert_eq!(arena.total_allocated(), 0);
    }

    #[test]
    fn test_allocation_profiler() {
        let mut profiler = AllocationProfiler::new();
        profiler.record_allocation("test".to_string(), 100);
        assert_eq!(profiler.total_allocated(), 100);
        assert_eq!(profiler.peak_allocated(), 100);

        profiler.record_allocation("test".to_string(), 50);
        assert_eq!(profiler.total_allocated(), 150);
        assert_eq!(profiler.peak_allocated(), 150);

        profiler.record_deallocation("test", 50);
        assert_eq!(profiler.total_allocated(), 100);
        assert_eq!(profiler.peak_allocated(), 150);
    }

    #[test]
    fn test_profiler_by_tag() {
        let mut profiler = AllocationProfiler::new();
        profiler.record_allocation("strings".to_string(), 100);
        profiler.record_allocation("vectors".to_string(), 200);
        
        let allocations = profiler.allocations_by_tag();
        assert_eq!(allocations.get("strings"), Some(&100));
        assert_eq!(allocations.get("vectors"), Some(&200));
    }
}
