//! Cross-language benchmarking infrastructure.
//!
//! Provides tools for comparing Kraken performance against other languages.

use std::collections::HashMap;
use std::process::Command;
use std::time::{Duration, Instant};

/// Supported languages for benchmarking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Language {
    Kraken,
    Rust,
    Go,
    Cpp,
    C,
}

impl Language {
    /// Get the language name as a string.
    pub fn name(&self) -> &'static str {
        match self {
            Language::Kraken => "Kraken",
            Language::Rust => "Rust",
            Language::Go => "Go",
            Language::Cpp => "C++",
            Language::C => "C",
        }
    }

    /// Get the file extension for this language.
    pub fn extension(&self) -> &'static str {
        match self {
            Language::Kraken => "kr",
            Language::Rust => "rs",
            Language::Go => "go",
            Language::Cpp => "cpp",
            Language::C => "c",
        }
    }

    /// Get the compiler command for this language.
    pub fn compiler(&self) -> &'static str {
        match self {
            Language::Kraken => "kraken",
            Language::Rust => "rustc",
            Language::Go => "go",
            Language::Cpp => "g++",
            Language::C => "gcc",
        }
    }
}

/// Benchmark category for organizing tests.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BenchmarkCategory {
    Algorithm,
    DataStructure,
    IO,
    Concurrency,
    Memory,
    Computation,
}

impl BenchmarkCategory {
    /// Get the category name as a string.
    pub fn name(&self) -> &'static str {
        match self {
            BenchmarkCategory::Algorithm => "Algorithm",
            BenchmarkCategory::DataStructure => "Data Structure",
            BenchmarkCategory::IO => "I/O",
            BenchmarkCategory::Concurrency => "Concurrency",
            BenchmarkCategory::Memory => "Memory",
            BenchmarkCategory::Computation => "Computation",
        }
    }
}

/// Metrics collected during benchmarking.
#[derive(Debug, Clone)]
pub struct BenchmarkMetrics {
    pub execution_time: Duration,
    pub compilation_time: Option<Duration>,
    pub memory_usage: Option<usize>,
    pub peak_memory: Option<usize>,
    pub binary_size: Option<usize>,
}

impl BenchmarkMetrics {
    /// Create new benchmark metrics.
    pub fn new(execution_time: Duration) -> Self {
        Self {
            execution_time,
            compilation_time: None,
            memory_usage: None,
            peak_memory: None,
            binary_size: None,
        }
    }

    /// Set compilation time.
    pub fn with_compilation_time(mut self, time: Duration) -> Self {
        self.compilation_time = Some(time);
        self
    }

    /// Set memory usage.
    pub fn with_memory_usage(mut self, bytes: usize) -> Self {
        self.memory_usage = Some(bytes);
        self
    }

    /// Set peak memory.
    pub fn with_peak_memory(mut self, bytes: usize) -> Self {
        self.peak_memory = Some(bytes);
        self
    }

    /// Set binary size.
    pub fn with_binary_size(mut self, bytes: usize) -> Self {
        self.binary_size = Some(bytes);
        self
    }
}

/// A single benchmark test.
#[derive(Debug, Clone)]
pub struct Benchmark {
    pub name: String,
    pub category: BenchmarkCategory,
    pub description: String,
    pub iterations: usize,
}

impl Benchmark {
    /// Create a new benchmark.
    pub fn new(name: String, category: BenchmarkCategory, description: String) -> Self {
        Self {
            name,
            category,
            description,
            iterations: 100,
        }
    }

    /// Set the number of iterations.
    pub fn with_iterations(mut self, iterations: usize) -> Self {
        self.iterations = iterations;
        self
    }
}

/// Results from running a benchmark across multiple languages.
#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    pub benchmark: Benchmark,
    pub results: HashMap<Language, BenchmarkMetrics>,
}

impl BenchmarkResult {
    /// Create a new benchmark result.
    pub fn new(benchmark: Benchmark) -> Self {
        Self {
            benchmark,
            results: HashMap::new(),
        }
    }

    /// Add a result for a language.
    pub fn add_result(&mut self, language: Language, metrics: BenchmarkMetrics) {
        self.results.insert(language, metrics);
    }

    /// Get the fastest language.
    pub fn fastest_language(&self) -> Option<Language> {
        self.results
            .iter()
            .min_by_key(|(_, m)| m.execution_time)
            .map(|(lang, _)| *lang)
    }

    /// Get the slowest language.
    pub fn slowest_language(&self) -> Option<Language> {
        self.results
            .iter()
            .max_by_key(|(_, m)| m.execution_time)
            .map(|(lang, _)| *lang)
    }

    /// Get speedup factor compared to a baseline language.
    pub fn speedup(&self, target: Language, baseline: Language) -> Option<f64> {
        let target_time = self.results.get(&target)?.execution_time.as_secs_f64();
        let baseline_time = self.results.get(&baseline)?.execution_time.as_secs_f64();
        Some(baseline_time / target_time)
    }

    /// Get relative performance as a percentage (100% = baseline).
    pub fn relative_performance(&self, target: Language, baseline: Language) -> Option<f64> {
        let target_time = self.results.get(&target)?.execution_time.as_secs_f64();
        let baseline_time = self.results.get(&baseline)?.execution_time.as_secs_f64();
        Some((target_time / baseline_time) * 100.0)
    }
}

/// Benchmark suite for running multiple benchmarks.
#[derive(Debug, Clone)]
pub struct BenchmarkSuite {
    pub name: String,
    pub benchmarks: Vec<Benchmark>,
    pub languages: Vec<Language>,
}

impl BenchmarkSuite {
    /// Create a new benchmark suite.
    pub fn new(name: String) -> Self {
        Self {
            name,
            benchmarks: Vec::new(),
            languages: Vec::new(),
        }
    }

    /// Add a benchmark to the suite.
    pub fn add_benchmark(&mut self, benchmark: Benchmark) {
        self.benchmarks.push(benchmark);
    }

    /// Add a language to test.
    pub fn add_language(&mut self, language: Language) {
        if !self.languages.contains(&language) {
            self.languages.push(language);
        }
    }

    /// Get the number of benchmarks.
    pub fn benchmark_count(&self) -> usize {
        self.benchmarks.len()
    }

    /// Get the number of languages.
    pub fn language_count(&self) -> usize {
        self.languages.len()
    }
}

/// Benchmark runner for executing benchmarks.
pub struct BenchmarkRunner {
    suite: BenchmarkSuite,
    results: Vec<BenchmarkResult>,
}

impl BenchmarkRunner {
    /// Create a new benchmark runner.
    pub fn new(suite: BenchmarkSuite) -> Self {
        Self {
            suite,
            results: Vec::new(),
        }
    }

    /// Run all benchmarks (stub implementation).
    pub fn run_all(&mut self) -> Result<(), String> {
        for benchmark in &self.suite.benchmarks {
            let mut result = BenchmarkResult::new(benchmark.clone());
            
            for &language in &self.suite.languages {
                // Stub: In real implementation, would compile and run actual code
                let metrics = self.run_benchmark_for_language(benchmark, language)?;
                result.add_result(language, metrics);
            }
            
            self.results.push(result);
        }
        Ok(())
    }

    /// Run a single benchmark for a specific language (stub).
    fn run_benchmark_for_language(
        &self,
        benchmark: &Benchmark,
        _language: Language,
    ) -> Result<BenchmarkMetrics, String> {
        // Stub implementation - would actually compile and run code
        let start = Instant::now();
        std::thread::sleep(Duration::from_micros(100)); // Simulate work
        let execution_time = start.elapsed();
        
        Ok(BenchmarkMetrics::new(execution_time)
            .with_compilation_time(Duration::from_millis(50))
            .with_memory_usage(1024 * benchmark.iterations)
            .with_peak_memory(2048 * benchmark.iterations)
            .with_binary_size(100000))
    }

    /// Get all results.
    pub fn results(&self) -> &[BenchmarkResult] {
        &self.results
    }

    /// Generate a comparison report.
    pub fn generate_report(&self, baseline: Language) -> String {
        let mut output = String::from("Benchmark Comparison Report\n");
        output.push_str("============================\n\n");
        output.push_str(&format!("Baseline: {}\n\n", baseline.name()));

        for result in &self.results {
            output.push_str(&format!("## {} ({})\n", result.benchmark.name, result.benchmark.category.name()));
            output.push_str(&format!("{}\n\n", result.benchmark.description));

            // Execution time comparison
            output.push_str("Execution Time:\n");
            let mut langs: Vec<_> = result.results.keys().collect();
            langs.sort_by_key(|l| result.results[l].execution_time);

            for lang in langs {
                let metrics = &result.results[lang];
                let time_ms = metrics.execution_time.as_secs_f64() * 1000.0;
                
                if let Some(rel_perf) = result.relative_performance(*lang, baseline) {
                    output.push_str(&format!(
                        "  {:8} {:8.2}ms  ({:6.1}% of baseline)\n",
                        lang.name(),
                        time_ms,
                        rel_perf
                    ));
                } else {
                    output.push_str(&format!("  {:8} {:8.2}ms\n", lang.name(), time_ms));
                }
            }
            output.push('\n');
        }

        output
    }

    /// Generate a summary table.
    pub fn generate_summary(&self, baseline: Language) -> String {
        let mut output = String::from("Performance Summary\n");
        output.push_str("===================\n\n");
        output.push_str(&format!("{:30} ", "Benchmark"));
        
        for lang in &self.suite.languages {
            output.push_str(&format!("{:12} ", lang.name()));
        }
        output.push('\n');
        output.push_str(&"-".repeat(30 + 12 * self.suite.languages.len()));
        output.push('\n');

        for result in &self.results {
            output.push_str(&format!("{:30} ", result.benchmark.name));
            
            for lang in &self.suite.languages {
                if let Some(metrics) = result.results.get(lang) {
                    let time_ms = metrics.execution_time.as_secs_f64() * 1000.0;
                    output.push_str(&format!("{:8.2}ms   ", time_ms));
                } else {
                    output.push_str("N/A          ");
                }
            }
            output.push('\n');
        }

        output
    }
}

/// Check if a compiler is available.
pub fn check_compiler_available(language: Language) -> bool {
    Command::new(language.compiler())
        .arg("--version")
        .output()
        .is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_language_properties() {
        assert_eq!(Language::Kraken.name(), "Kraken");
        assert_eq!(Language::Rust.extension(), "rs");
        assert_eq!(Language::Go.compiler(), "go");
    }

    #[test]
    fn test_benchmark_category() {
        assert_eq!(BenchmarkCategory::Algorithm.name(), "Algorithm");
        assert_eq!(BenchmarkCategory::IO.name(), "I/O");
    }

    #[test]
    fn test_benchmark_metrics() {
        let metrics = BenchmarkMetrics::new(Duration::from_millis(100))
            .with_compilation_time(Duration::from_millis(50))
            .with_memory_usage(1024)
            .with_peak_memory(2048)
            .with_binary_size(100000);

        assert_eq!(metrics.execution_time, Duration::from_millis(100));
        assert_eq!(metrics.compilation_time, Some(Duration::from_millis(50)));
        assert_eq!(metrics.memory_usage, Some(1024));
        assert_eq!(metrics.peak_memory, Some(2048));
        assert_eq!(metrics.binary_size, Some(100000));
    }

    #[test]
    fn test_benchmark_creation() {
        let bench = Benchmark::new(
            "fibonacci".to_string(),
            BenchmarkCategory::Algorithm,
            "Calculate Fibonacci numbers".to_string(),
        ).with_iterations(1000);

        assert_eq!(bench.name, "fibonacci");
        assert_eq!(bench.iterations, 1000);
    }

    #[test]
    fn test_benchmark_result() {
        let bench = Benchmark::new(
            "test".to_string(),
            BenchmarkCategory::Algorithm,
            "Test benchmark".to_string(),
        );
        
        let mut result = BenchmarkResult::new(bench);
        result.add_result(Language::Kraken, BenchmarkMetrics::new(Duration::from_millis(100)));
        result.add_result(Language::Rust, BenchmarkMetrics::new(Duration::from_millis(90)));
        result.add_result(Language::Go, BenchmarkMetrics::new(Duration::from_millis(110)));

        assert_eq!(result.fastest_language(), Some(Language::Rust));
        assert_eq!(result.slowest_language(), Some(Language::Go));
    }

    #[test]
    fn test_speedup_calculation() {
        let bench = Benchmark::new(
            "test".to_string(),
            BenchmarkCategory::Algorithm,
            "Test".to_string(),
        );
        
        let mut result = BenchmarkResult::new(bench);
        result.add_result(Language::Kraken, BenchmarkMetrics::new(Duration::from_millis(100)));
        result.add_result(Language::Rust, BenchmarkMetrics::new(Duration::from_millis(50)));

        let speedup = result.speedup(Language::Rust, Language::Kraken).unwrap();
        assert!((speedup - 2.0).abs() < 0.01);
    }

    #[test]
    fn test_relative_performance() {
        let bench = Benchmark::new(
            "test".to_string(),
            BenchmarkCategory::Algorithm,
            "Test".to_string(),
        );
        
        let mut result = BenchmarkResult::new(bench);
        result.add_result(Language::Kraken, BenchmarkMetrics::new(Duration::from_millis(100)));
        result.add_result(Language::Rust, BenchmarkMetrics::new(Duration::from_millis(50)));

        let rel_perf = result.relative_performance(Language::Rust, Language::Kraken).unwrap();
        assert!((rel_perf - 50.0).abs() < 0.01);
    }

    #[test]
    fn test_benchmark_suite() {
        let mut suite = BenchmarkSuite::new("Test Suite".to_string());
        
        suite.add_benchmark(Benchmark::new(
            "test1".to_string(),
            BenchmarkCategory::Algorithm,
            "Test 1".to_string(),
        ));
        
        suite.add_language(Language::Kraken);
        suite.add_language(Language::Rust);
        suite.add_language(Language::Rust); // Duplicate

        assert_eq!(suite.benchmark_count(), 1);
        assert_eq!(suite.language_count(), 2);
    }

    #[test]
    fn test_benchmark_runner() {
        let mut suite = BenchmarkSuite::new("Test Suite".to_string());
        suite.add_benchmark(Benchmark::new(
            "test".to_string(),
            BenchmarkCategory::Algorithm,
            "Test".to_string(),
        ));
        suite.add_language(Language::Kraken);
        suite.add_language(Language::Rust);

        let mut runner = BenchmarkRunner::new(suite);
        assert!(runner.run_all().is_ok());
        assert_eq!(runner.results().len(), 1);
    }

    #[test]
    fn test_generate_report() {
        let mut suite = BenchmarkSuite::new("Test Suite".to_string());
        suite.add_benchmark(Benchmark::new(
            "test".to_string(),
            BenchmarkCategory::Algorithm,
            "Test benchmark".to_string(),
        ));
        suite.add_language(Language::Kraken);
        suite.add_language(Language::Rust);

        let mut runner = BenchmarkRunner::new(suite);
        runner.run_all().unwrap();

        let report = runner.generate_report(Language::Kraken);
        assert!(report.contains("Benchmark Comparison Report"));
        assert!(report.contains("Baseline: Kraken"));
    }

    #[test]
    fn test_generate_summary() {
        let mut suite = BenchmarkSuite::new("Test Suite".to_string());
        suite.add_benchmark(Benchmark::new(
            "test".to_string(),
            BenchmarkCategory::Algorithm,
            "Test".to_string(),
        ));
        suite.add_language(Language::Kraken);

        let mut runner = BenchmarkRunner::new(suite);
        runner.run_all().unwrap();

        let summary = runner.generate_summary(Language::Kraken);
        assert!(summary.contains("Performance Summary"));
        assert!(summary.contains("Kraken"));
    }
}
