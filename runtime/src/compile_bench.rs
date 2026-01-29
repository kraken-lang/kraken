//! Compile-time benchmarking infrastructure.
//!
//! Provides tools for measuring and tracking compilation performance.

use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Benchmark measurement for a compilation phase.
#[derive(Debug, Clone)]
pub struct BenchmarkMeasurement {
    pub name: String,
    pub duration: Duration,
    pub memory_bytes: usize,
}

impl BenchmarkMeasurement {
    /// Create a new measurement.
    pub fn new(name: String, duration: Duration, memory_bytes: usize) -> Self {
        Self {
            name,
            duration,
            memory_bytes,
        }
    }

    /// Get duration in milliseconds.
    pub fn duration_ms(&self) -> f64 {
        self.duration.as_secs_f64() * 1000.0
    }

    /// Get memory in megabytes.
    pub fn memory_mb(&self) -> f64 {
        self.memory_bytes as f64 / (1024.0 * 1024.0)
    }
}

/// Benchmark tracker for compilation phases.
#[derive(Debug, Clone)]
pub struct CompilationBenchmark {
    measurements: HashMap<String, BenchmarkMeasurement>,
    start_times: HashMap<String, Instant>,
}

impl CompilationBenchmark {
    /// Create a new benchmark tracker.
    pub fn new() -> Self {
        Self {
            measurements: HashMap::new(),
            start_times: HashMap::new(),
        }
    }

    /// Start timing a phase.
    pub fn start_phase(&mut self, name: &str) {
        self.start_times.insert(name.to_string(), Instant::now());
    }

    /// End timing a phase.
    pub fn end_phase(&mut self, name: &str, memory_bytes: usize) {
        if let Some(start) = self.start_times.remove(name) {
            let duration = start.elapsed();
            let measurement = BenchmarkMeasurement::new(name.to_string(), duration, memory_bytes);
            self.measurements.insert(name.to_string(), measurement);
        }
    }

    /// Get a measurement by name.
    pub fn get_measurement(&self, name: &str) -> Option<&BenchmarkMeasurement> {
        self.measurements.get(name)
    }

    /// Get all measurements.
    pub fn measurements(&self) -> &HashMap<String, BenchmarkMeasurement> {
        &self.measurements
    }

    /// Get total compilation time.
    pub fn total_time(&self) -> Duration {
        self.measurements.values().map(|m| m.duration).sum()
    }

    /// Get total memory usage.
    pub fn total_memory(&self) -> usize {
        self.measurements.values().map(|m| m.memory_bytes).sum()
    }

    /// Print benchmark report.
    pub fn print_report(&self) -> String {
        let mut output = String::from("Compilation Benchmark Report\n");
        output.push_str("=============================\n\n");

        let mut phases: Vec<_> = self.measurements.values().collect();
        phases.sort_by(|a, b| b.duration.cmp(&a.duration));

        for measurement in phases {
            output.push_str(&format!(
                "{:20} {:8.2}ms  {:8.2}MB\n",
                measurement.name,
                measurement.duration_ms(),
                measurement.memory_mb()
            ));
        }

        output.push_str(&format!(
            "\nTotal: {:8.2}ms  {:8.2}MB\n",
            self.total_time().as_secs_f64() * 1000.0,
            self.total_memory() as f64 / (1024.0 * 1024.0)
        ));

        output
    }
}

impl Default for CompilationBenchmark {
    fn default() -> Self {
        Self::new()
    }
}

/// Memory usage tracker.
pub struct MemoryTracker {
    peak_usage: usize,
    current_usage: usize,
}

impl MemoryTracker {
    /// Create a new memory tracker.
    pub fn new() -> Self {
        Self {
            peak_usage: 0,
            current_usage: 0,
        }
    }

    /// Allocate memory.
    pub fn allocate(&mut self, bytes: usize) {
        self.current_usage += bytes;
        if self.current_usage > self.peak_usage {
            self.peak_usage = self.current_usage;
        }
    }

    /// Deallocate memory.
    pub fn deallocate(&mut self, bytes: usize) {
        self.current_usage = self.current_usage.saturating_sub(bytes);
    }

    /// Get current memory usage.
    pub fn current_usage(&self) -> usize {
        self.current_usage
    }

    /// Get peak memory usage.
    pub fn peak_usage(&self) -> usize {
        self.peak_usage
    }

    /// Reset the tracker.
    pub fn reset(&mut self) {
        self.peak_usage = 0;
        self.current_usage = 0;
    }
}

impl Default for MemoryTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Performance statistics for compilation.
#[derive(Debug, Clone)]
pub struct PerformanceStats {
    pub files_compiled: usize,
    pub lines_compiled: usize,
    pub total_time: Duration,
    pub peak_memory: usize,
}

impl PerformanceStats {
    /// Create new performance stats.
    pub fn new() -> Self {
        Self {
            files_compiled: 0,
            lines_compiled: 0,
            total_time: Duration::ZERO,
            peak_memory: 0,
        }
    }

    /// Get lines per second.
    pub fn lines_per_second(&self) -> f64 {
        if self.total_time.as_secs_f64() > 0.0 {
            self.lines_compiled as f64 / self.total_time.as_secs_f64()
        } else {
            0.0
        }
    }

    /// Get files per second.
    pub fn files_per_second(&self) -> f64 {
        if self.total_time.as_secs_f64() > 0.0 {
            self.files_compiled as f64 / self.total_time.as_secs_f64()
        } else {
            0.0
        }
    }
}

impl Default for PerformanceStats {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_benchmark_measurement() {
        let measurement =
            BenchmarkMeasurement::new("test".to_string(), Duration::from_millis(100), 1024 * 1024);
        assert_eq!(measurement.name, "test");
        assert!(measurement.duration_ms() >= 100.0);
        assert!(measurement.memory_mb() >= 1.0);
    }

    #[test]
    fn test_compilation_benchmark() {
        let mut bench = CompilationBenchmark::new();

        bench.start_phase("lexing");
        thread::sleep(Duration::from_millis(10));
        bench.end_phase("lexing", 1024);

        let measurement = bench.get_measurement("lexing");
        assert!(measurement.is_some());
        assert!(measurement.unwrap().duration >= Duration::from_millis(10));
    }

    #[test]
    fn test_benchmark_total_time() {
        let mut bench = CompilationBenchmark::new();

        bench.start_phase("phase1");
        thread::sleep(Duration::from_millis(10));
        bench.end_phase("phase1", 1024);

        bench.start_phase("phase2");
        thread::sleep(Duration::from_millis(10));
        bench.end_phase("phase2", 2048);

        let total = bench.total_time();
        assert!(total >= Duration::from_millis(20));
    }

    #[test]
    fn test_benchmark_total_memory() {
        let mut bench = CompilationBenchmark::new();

        bench.start_phase("phase1");
        bench.end_phase("phase1", 1024);

        bench.start_phase("phase2");
        bench.end_phase("phase2", 2048);

        assert_eq!(bench.total_memory(), 3072);
    }

    #[test]
    fn test_memory_tracker() {
        let mut tracker = MemoryTracker::new();
        assert_eq!(tracker.current_usage(), 0);
        assert_eq!(tracker.peak_usage(), 0);

        tracker.allocate(1024);
        assert_eq!(tracker.current_usage(), 1024);
        assert_eq!(tracker.peak_usage(), 1024);

        tracker.allocate(2048);
        assert_eq!(tracker.current_usage(), 3072);
        assert_eq!(tracker.peak_usage(), 3072);

        tracker.deallocate(1024);
        assert_eq!(tracker.current_usage(), 2048);
        assert_eq!(tracker.peak_usage(), 3072);
    }

    #[test]
    fn test_memory_tracker_reset() {
        let mut tracker = MemoryTracker::new();
        tracker.allocate(1024);
        tracker.reset();
        assert_eq!(tracker.current_usage(), 0);
        assert_eq!(tracker.peak_usage(), 0);
    }

    #[test]
    fn test_performance_stats() {
        let mut stats = PerformanceStats::new();
        stats.files_compiled = 10;
        stats.lines_compiled = 1000;
        stats.total_time = Duration::from_secs(1);

        assert_eq!(stats.lines_per_second(), 1000.0);
        assert_eq!(stats.files_per_second(), 10.0);
    }

    #[test]
    fn test_benchmark_print_report() {
        let mut bench = CompilationBenchmark::new();
        bench.start_phase("lexing");
        bench.end_phase("lexing", 1024);

        let report = bench.print_report();
        assert!(report.contains("Compilation Benchmark Report"));
        assert!(report.contains("lexing"));
    }
}
