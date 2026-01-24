//! Thread safety utilities including deadlock detection and performance monitoring.

#![allow(dead_code)]

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::thread::ThreadId;
use std::time::{Duration, Instant};

/// Deadlock detector for monitoring lock acquisition patterns
pub struct DeadlockDetector {
    lock_graph: Arc<Mutex<HashMap<ThreadId, Vec<String>>>>,
    enabled: bool,
}

impl DeadlockDetector {
    pub fn new() -> Self {
        Self {
            lock_graph: Arc::new(Mutex::new(HashMap::new())),
            enabled: true,
        }
    }

    pub fn enable(&mut self) {
        self.enabled = true;
    }

    pub fn disable(&mut self) {
        self.enabled = false;
    }

    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    pub fn register_lock_acquisition(&self, lock_name: String) {
        if !self.enabled {
            return;
        }

        let thread_id = std::thread::current().id();
        let mut graph = self.lock_graph.lock().unwrap();
        graph.entry(thread_id).or_default().push(lock_name);
    }

    pub fn register_lock_release(&self, lock_name: &str) {
        if !self.enabled {
            return;
        }

        let thread_id = std::thread::current().id();
        let mut graph = self.lock_graph.lock().unwrap();
        if let Some(locks) = graph.get_mut(&thread_id) {
            locks.retain(|l| l != lock_name);
            if locks.is_empty() {
                graph.remove(&thread_id);
            }
        }
    }

    pub fn check_for_cycles(&self) -> Vec<String> {
        if !self.enabled {
            return Vec::new();
        }

        let graph = self.lock_graph.lock().unwrap();
        let mut potential_deadlocks = Vec::new();

        for (thread_id, locks) in graph.iter() {
            if locks.len() > 1 {
                potential_deadlocks.push(format!(
                    "Thread {thread_id:?} holds multiple locks: {locks:?}"
                ));
            }
        }

        potential_deadlocks
    }

    pub fn clear(&self) {
        let mut graph = self.lock_graph.lock().unwrap();
        graph.clear();
    }
}

impl Default for DeadlockDetector {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for DeadlockDetector {
    fn clone(&self) -> Self {
        Self {
            lock_graph: self.lock_graph.clone(),
            enabled: self.enabled,
        }
    }
}

/// Performance metrics for concurrent operations
#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    pub lock_acquisitions: u64,
    pub lock_contentions: u64,
    pub total_wait_time: Duration,
    pub max_wait_time: Duration,
    pub thread_count: usize,
}

impl PerformanceMetrics {
    pub fn new() -> Self {
        Self {
            lock_acquisitions: 0,
            lock_contentions: 0,
            total_wait_time: Duration::from_secs(0),
            max_wait_time: Duration::from_secs(0),
            thread_count: 0,
        }
    }

    pub fn contention_rate(&self) -> f64 {
        if self.lock_acquisitions == 0 {
            0.0
        } else {
            self.lock_contentions as f64 / self.lock_acquisitions as f64
        }
    }

    pub fn average_wait_time(&self) -> Duration {
        if self.lock_contentions == 0 {
            Duration::from_secs(0)
        } else {
            self.total_wait_time / self.lock_contentions as u32
        }
    }
}

impl Default for PerformanceMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// Performance monitor for tracking concurrent operations
pub struct PerformanceMonitor {
    metrics: Arc<Mutex<PerformanceMetrics>>,
    enabled: bool,
}

impl PerformanceMonitor {
    pub fn new() -> Self {
        Self {
            metrics: Arc::new(Mutex::new(PerformanceMetrics::new())),
            enabled: true,
        }
    }

    pub fn enable(&mut self) {
        self.enabled = true;
    }

    pub fn disable(&mut self) {
        self.enabled = false;
    }

    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    pub fn record_lock_acquisition(&self) {
        if !self.enabled {
            return;
        }

        let mut metrics = self.metrics.lock().unwrap();
        metrics.lock_acquisitions += 1;
    }

    pub fn record_lock_contention(&self, wait_time: Duration) {
        if !self.enabled {
            return;
        }

        let mut metrics = self.metrics.lock().unwrap();
        metrics.lock_contentions += 1;
        metrics.total_wait_time += wait_time;
        if wait_time > metrics.max_wait_time {
            metrics.max_wait_time = wait_time;
        }
    }

    pub fn record_thread_spawn(&self) {
        if !self.enabled {
            return;
        }

        let mut metrics = self.metrics.lock().unwrap();
        metrics.thread_count += 1;
    }

    pub fn get_metrics(&self) -> PerformanceMetrics {
        let metrics = self.metrics.lock().unwrap();
        metrics.clone()
    }

    pub fn reset(&self) {
        let mut metrics = self.metrics.lock().unwrap();
        *metrics = PerformanceMetrics::new();
    }
}

impl Default for PerformanceMonitor {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for PerformanceMonitor {
    fn clone(&self) -> Self {
        Self {
            metrics: self.metrics.clone(),
            enabled: self.enabled,
        }
    }
}

/// Utility function to try acquiring a lock with timeout
pub fn try_lock_with_timeout<T>(
    mutex: &Mutex<T>,
    timeout: Duration,
) -> Option<std::sync::MutexGuard<T>> {
    let start = Instant::now();

    loop {
        if let Ok(guard) = mutex.try_lock() {
            return Some(guard);
        }

        if start.elapsed() >= timeout {
            return None;
        }

        std::thread::yield_now();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_deadlock_detector_creation() {
        let detector = DeadlockDetector::new();
        assert!(detector.is_enabled());
    }

    #[test]
    fn test_deadlock_detector_enable_disable() {
        let mut detector = DeadlockDetector::new();
        assert!(detector.is_enabled());

        detector.disable();
        assert!(!detector.is_enabled());

        detector.enable();
        assert!(detector.is_enabled());
    }

    #[test]
    fn test_deadlock_detector_register_locks() {
        let detector = DeadlockDetector::new();
        detector.register_lock_acquisition("lock1".to_string());
        detector.register_lock_acquisition("lock2".to_string());

        let cycles = detector.check_for_cycles();
        assert!(!cycles.is_empty());

        detector.register_lock_release("lock1");
        detector.register_lock_release("lock2");
    }

    #[test]
    fn test_deadlock_detector_clear() {
        let detector = DeadlockDetector::new();
        detector.register_lock_acquisition("lock1".to_string());
        detector.clear();

        let cycles = detector.check_for_cycles();
        assert!(cycles.is_empty());
    }

    #[test]
    fn test_performance_monitor_creation() {
        let monitor = PerformanceMonitor::new();
        assert!(monitor.is_enabled());
    }

    #[test]
    fn test_performance_monitor_record_acquisition() {
        let monitor = PerformanceMonitor::new();
        monitor.record_lock_acquisition();
        monitor.record_lock_acquisition();

        let metrics = monitor.get_metrics();
        assert_eq!(metrics.lock_acquisitions, 2);
    }

    #[test]
    fn test_performance_monitor_record_contention() {
        let monitor = PerformanceMonitor::new();
        monitor.record_lock_contention(Duration::from_millis(10));
        monitor.record_lock_contention(Duration::from_millis(20));

        let metrics = monitor.get_metrics();
        assert_eq!(metrics.lock_contentions, 2);
        assert_eq!(metrics.max_wait_time, Duration::from_millis(20));
    }

    #[test]
    fn test_performance_metrics_contention_rate() {
        let mut metrics = PerformanceMetrics::new();
        metrics.lock_acquisitions = 100;
        metrics.lock_contentions = 10;

        assert_eq!(metrics.contention_rate(), 0.1);
    }

    #[test]
    fn test_performance_monitor_reset() {
        let monitor = PerformanceMonitor::new();
        monitor.record_lock_acquisition();
        monitor.reset();

        let metrics = monitor.get_metrics();
        assert_eq!(metrics.lock_acquisitions, 0);
    }

    #[test]
    fn test_performance_monitor_thread_spawn() {
        let monitor = PerformanceMonitor::new();
        monitor.record_thread_spawn();
        monitor.record_thread_spawn();

        let metrics = monitor.get_metrics();
        assert_eq!(metrics.thread_count, 2);
    }

    #[test]
    fn test_deadlock_detector_concurrent() {
        let detector = DeadlockDetector::new();
        let detector_clone = detector.clone();

        let handle = thread::spawn(move || {
            detector_clone.register_lock_acquisition("thread_lock".to_string());
            detector_clone.register_lock_release("thread_lock");
        });

        handle.join().unwrap();
    }
}
