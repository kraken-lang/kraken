use std::collections::HashSet;
use std::sync::{Arc, RwLock};

/// Garbage collector for development mode.
/// 
/// Provides optional garbage collection warnings for leaked variables
/// in development mode. This is NOT a full GC implementation - Kraken
/// uses manual memory management by default.
pub struct GarbageCollector {
    /// Track allocated objects
    tracked_allocations: Arc<RwLock<HashSet<usize>>>,
    /// Enable GC warnings
    warnings_enabled: bool,
}

impl GarbageCollector {
    /// Create a new garbage collector.
    /// 
    /// # Arguments
    /// * `warnings_enabled` - Enable warnings for leaked allocations
    pub fn new(warnings_enabled: bool) -> Self {
        Self {
            tracked_allocations: Arc::new(RwLock::new(HashSet::new())),
            warnings_enabled,
        }
    }

    /// Track a new allocation.
    /// 
    /// # Arguments
    /// * `ptr` - Pointer address to track
    pub fn track_allocation(&self, ptr: usize) {
        if self.warnings_enabled {
            if let Ok(mut allocations) = self.tracked_allocations.write() {
                allocations.insert(ptr);
            }
        }
    }

    /// Untrack a freed allocation.
    /// 
    /// # Arguments
    /// * `ptr` - Pointer address to untrack
    pub fn untrack_allocation(&self, ptr: usize) {
        if self.warnings_enabled {
            if let Ok(mut allocations) = self.tracked_allocations.write() {
                allocations.remove(&ptr);
            }
        }
    }

    /// Check for leaked allocations and emit warnings.
    /// 
    /// # Returns
    /// Number of leaked allocations detected
    pub fn check_leaks(&self) -> usize {
        if !self.warnings_enabled {
            return 0;
        }

        if let Ok(allocations) = self.tracked_allocations.read() {
            let leak_count = allocations.len();
            
            if leak_count > 0 {
                eprintln!("Warning: {leak_count} potential memory leak(s) detected");
                
                for (idx, ptr) in allocations.iter().enumerate().take(10) {
                    eprintln!("  Leak {}: allocation at 0x{ptr:x}", idx + 1);
                }
                
                if leak_count > 10 {
                    eprintln!("  ... and {} more", leak_count - 10);
                }
            }
            
            leak_count
        } else {
            0
        }
    }

    /// Clear all tracked allocations.
    pub fn clear(&self) {
        if let Ok(mut allocations) = self.tracked_allocations.write() {
            allocations.clear();
        }
    }

    /// Get the number of currently tracked allocations.
    pub fn tracked_count(&self) -> usize {
        if let Ok(allocations) = self.tracked_allocations.read() {
            allocations.len()
        } else {
            0
        }
    }

    /// Check if warnings are enabled.
    pub fn warnings_enabled(&self) -> bool {
        self.warnings_enabled
    }
}

impl Default for GarbageCollector {
    fn default() -> Self {
        Self::new(false)
    }
}

impl Drop for GarbageCollector {
    fn drop(&mut self) {
        if self.warnings_enabled {
            self.check_leaks();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_track_and_untrack() {
        let gc = GarbageCollector::new(true);
        
        let ptr1 = 0x1000;
        let ptr2 = 0x2000;
        
        gc.track_allocation(ptr1);
        gc.track_allocation(ptr2);
        assert_eq!(gc.tracked_count(), 2);
        
        gc.untrack_allocation(ptr1);
        assert_eq!(gc.tracked_count(), 1);
        
        gc.untrack_allocation(ptr2);
        assert_eq!(gc.tracked_count(), 0);
    }

    #[test]
    fn test_leak_detection() {
        let gc = GarbageCollector::new(true);
        
        gc.track_allocation(0x1000);
        gc.track_allocation(0x2000);
        
        let leaks = gc.check_leaks();
        assert_eq!(leaks, 2);
    }

    #[test]
    fn test_warnings_disabled() {
        let gc = GarbageCollector::new(false);
        assert!(!gc.warnings_enabled());
        
        gc.track_allocation(0x1000);
        assert_eq!(gc.tracked_count(), 0);
        
        let leaks = gc.check_leaks();
        assert_eq!(leaks, 0);
    }

    #[test]
    fn test_clear() {
        let gc = GarbageCollector::new(true);
        
        gc.track_allocation(0x1000);
        gc.track_allocation(0x2000);
        assert_eq!(gc.tracked_count(), 2);
        
        gc.clear();
        assert_eq!(gc.tracked_count(), 0);
    }
}
