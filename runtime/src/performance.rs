//! Performance profiling and optimization infrastructure.
//!
//! Provides tools for measuring and optimizing runtime performance.

use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Performance profiler for tracking execution time.
#[derive(Debug, Clone)]
pub struct PerformanceProfiler {
    measurements: HashMap<String, Vec<Duration>>,
    active_timers: HashMap<String, Instant>,
}

impl PerformanceProfiler {
    /// Create a new performance profiler.
    pub fn new() -> Self {
        Self {
            measurements: HashMap::new(),
            active_timers: HashMap::new(),
        }
    }

    /// Start timing a section.
    pub fn start(&mut self, name: &str) {
        self.active_timers.insert(name.to_string(), Instant::now());
    }

    /// Stop timing a section and record the duration.
    pub fn stop(&mut self, name: &str) {
        if let Some(start) = self.active_timers.remove(name) {
            let duration = start.elapsed();
            self.measurements
                .entry(name.to_string())
                .or_default()
                .push(duration);
        }
    }

    /// Get all measurements for a section.
    pub fn get_measurements(&self, name: &str) -> Option<&Vec<Duration>> {
        self.measurements.get(name)
    }

    /// Get average duration for a section.
    pub fn average_duration(&self, name: &str) -> Option<Duration> {
        self.measurements.get(name).and_then(|durations| {
            if durations.is_empty() {
                None
            } else {
                let total: Duration = durations.iter().sum();
                Some(total / durations.len() as u32)
            }
        })
    }

    /// Get minimum duration for a section.
    pub fn min_duration(&self, name: &str) -> Option<Duration> {
        self.measurements
            .get(name)
            .and_then(|durations| durations.iter().min().copied())
    }

    /// Get maximum duration for a section.
    pub fn max_duration(&self, name: &str) -> Option<Duration> {
        self.measurements
            .get(name)
            .and_then(|durations| durations.iter().max().copied())
    }

    /// Get total number of measurements for a section.
    pub fn measurement_count(&self, name: &str) -> usize {
        self.measurements.get(name).map(|d| d.len()).unwrap_or(0)
    }

    /// Clear all measurements.
    pub fn clear(&mut self) {
        self.measurements.clear();
        self.active_timers.clear();
    }

    /// Generate a performance report.
    pub fn report(&self) -> String {
        let mut output = String::from("Performance Profile Report\n");
        output.push_str("===========================\n\n");

        let mut sections: Vec<_> = self.measurements.keys().collect();
        sections.sort();

        for section in sections {
            if let Some(avg) = self.average_duration(section) {
                let min = self.min_duration(section).unwrap();
                let max = self.max_duration(section).unwrap();
                let count = self.measurement_count(section);

                output.push_str(&format!(
                    "{:30} Count: {:6}  Avg: {:8.2}ms  Min: {:8.2}ms  Max: {:8.2}ms\n",
                    section,
                    count,
                    avg.as_secs_f64() * 1000.0,
                    min.as_secs_f64() * 1000.0,
                    max.as_secs_f64() * 1000.0
                ));
            }
        }

        output
    }
}

impl Default for PerformanceProfiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Memory allocation tracker.
#[derive(Debug, Clone)]
pub struct AllocationTracker {
    allocations: HashMap<String, usize>,
    deallocations: HashMap<String, usize>,
    peak_usage: HashMap<String, usize>,
}

impl AllocationTracker {
    /// Create a new allocation tracker.
    pub fn new() -> Self {
        Self {
            allocations: HashMap::new(),
            deallocations: HashMap::new(),
            peak_usage: HashMap::new(),
        }
    }

    /// Record an allocation.
    pub fn allocate(&mut self, category: &str, bytes: usize) {
        *self.allocations.entry(category.to_string()).or_insert(0) += bytes;

        let current = self.current_usage(category);
        let peak = self.peak_usage.entry(category.to_string()).or_insert(0);
        if current > *peak {
            *peak = current;
        }
    }

    /// Record a deallocation.
    pub fn deallocate(&mut self, category: &str, bytes: usize) {
        *self.deallocations.entry(category.to_string()).or_insert(0) += bytes;
    }

    /// Get current memory usage for a category.
    pub fn current_usage(&self, category: &str) -> usize {
        let allocated = self.allocations.get(category).copied().unwrap_or(0);
        let deallocated = self.deallocations.get(category).copied().unwrap_or(0);
        allocated.saturating_sub(deallocated)
    }

    /// Get peak memory usage for a category.
    pub fn peak_usage(&self, category: &str) -> usize {
        self.peak_usage.get(category).copied().unwrap_or(0)
    }

    /// Get total allocated bytes for a category.
    pub fn total_allocated(&self, category: &str) -> usize {
        self.allocations.get(category).copied().unwrap_or(0)
    }

    /// Get total deallocated bytes for a category.
    pub fn total_deallocated(&self, category: &str) -> usize {
        self.deallocations.get(category).copied().unwrap_or(0)
    }

    /// Clear all tracking data.
    pub fn clear(&mut self) {
        self.allocations.clear();
        self.deallocations.clear();
        self.peak_usage.clear();
    }

    /// Generate an allocation report.
    pub fn report(&self) -> String {
        let mut output = String::from("Memory Allocation Report\n");
        output.push_str("========================\n\n");

        let mut categories: Vec<_> = self.allocations.keys().collect();
        categories.sort();

        for category in categories {
            let allocated = self.total_allocated(category);
            let deallocated = self.total_deallocated(category);
            let current = self.current_usage(category);
            let peak = self.peak_usage(category);

            output.push_str(&format!(
                "{category:20} Allocated: {allocated:10} bytes  Deallocated: {deallocated:10} bytes  Current: {current:10} bytes  Peak: {peak:10} bytes\n"
            ));
        }

        output
    }
}

impl Default for AllocationTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Cache performance metrics.
#[derive(Debug, Clone, Default)]
pub struct CacheMetrics {
    pub hits: usize,
    pub misses: usize,
    pub evictions: usize,
}

impl CacheMetrics {
    /// Create new cache metrics.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a cache hit.
    pub fn record_hit(&mut self) {
        self.hits += 1;
    }

    /// Record a cache miss.
    pub fn record_miss(&mut self) {
        self.misses += 1;
    }

    /// Record a cache eviction.
    pub fn record_eviction(&mut self) {
        self.evictions += 1;
    }

    /// Get hit rate as a percentage.
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            (self.hits as f64 / total as f64) * 100.0
        }
    }

    /// Get miss rate as a percentage.
    pub fn miss_rate(&self) -> f64 {
        100.0 - self.hit_rate()
    }

    /// Clear all metrics.
    pub fn clear(&mut self) {
        self.hits = 0;
        self.misses = 0;
        self.evictions = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_performance_profiler() {
        let mut profiler = PerformanceProfiler::new();

        profiler.start("test");
        thread::sleep(Duration::from_millis(10));
        profiler.stop("test");

        assert_eq!(profiler.measurement_count("test"), 1);
        let avg = profiler.average_duration("test").unwrap();
        assert!(avg >= Duration::from_millis(10));
    }

    #[test]
    fn test_profiler_multiple_measurements() {
        let mut profiler = PerformanceProfiler::new();

        for _ in 0..5 {
            profiler.start("loop");
            thread::sleep(Duration::from_millis(5));
            profiler.stop("loop");
        }

        assert_eq!(profiler.measurement_count("loop"), 5);
        assert!(profiler.average_duration("loop").is_some());
        assert!(profiler.min_duration("loop").is_some());
        assert!(profiler.max_duration("loop").is_some());
    }

    #[test]
    fn test_profiler_clear() {
        let mut profiler = PerformanceProfiler::new();
        profiler.start("test");
        profiler.stop("test");

        profiler.clear();
        assert_eq!(profiler.measurement_count("test"), 0);
    }

    #[test]
    fn test_allocation_tracker() {
        let mut tracker = AllocationTracker::new();

        tracker.allocate("test", 1024);
        assert_eq!(tracker.current_usage("test"), 1024);
        assert_eq!(tracker.peak_usage("test"), 1024);

        tracker.allocate("test", 2048);
        assert_eq!(tracker.current_usage("test"), 3072);
        assert_eq!(tracker.peak_usage("test"), 3072);

        tracker.deallocate("test", 1024);
        assert_eq!(tracker.current_usage("test"), 2048);
        assert_eq!(tracker.peak_usage("test"), 3072);
    }

    #[test]
    fn test_allocation_tracker_multiple_categories() {
        let mut tracker = AllocationTracker::new();

        tracker.allocate("cat1", 1000);
        tracker.allocate("cat2", 2000);

        assert_eq!(tracker.current_usage("cat1"), 1000);
        assert_eq!(tracker.current_usage("cat2"), 2000);
    }

    #[test]
    fn test_cache_metrics() {
        let mut metrics = CacheMetrics::new();

        metrics.record_hit();
        metrics.record_hit();
        metrics.record_miss();

        assert_eq!(metrics.hits, 2);
        assert_eq!(metrics.misses, 1);
        assert!((metrics.hit_rate() - 66.67).abs() < 0.1);
    }

    #[test]
    fn test_cache_metrics_empty() {
        let metrics = CacheMetrics::new();
        assert_eq!(metrics.hit_rate(), 0.0);
        assert_eq!(metrics.miss_rate(), 100.0);
    }

    #[test]
    fn test_profiler_report() {
        let mut profiler = PerformanceProfiler::new();
        profiler.start("test");
        profiler.stop("test");

        let report = profiler.report();
        assert!(report.contains("Performance Profile Report"));
        assert!(report.contains("test"));
    }

    #[test]
    fn test_allocation_report() {
        let mut tracker = AllocationTracker::new();
        tracker.allocate("test", 1024);

        let report = tracker.report();
        assert!(report.contains("Memory Allocation Report"));
        assert!(report.contains("test"));
    }
}
