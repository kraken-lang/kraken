//! Bench command - Run benchmarks with statistical analysis.

use crate::cli::output::{OutputMessage, ProgressIndicator};
use crate::cli::{Command, CommandResult};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

#[derive(Debug, Serialize, Deserialize)]
pub struct BenchmarkResults {
    pub name: String,
    pub iterations: usize,
    pub mean: f64,
    pub median: f64,
    pub std_dev: f64,
    pub min: f64,
    pub max: f64,
}

impl BenchmarkResults {
    fn from_samples(name: String, samples: &[Duration]) -> Self {
        let mut times: Vec<f64> = samples.iter().map(|d| d.as_secs_f64() * 1000.0).collect();
        times.sort_by(|a, b| a.partial_cmp(b).unwrap());

        let mean = times.iter().sum::<f64>() / times.len() as f64;
        let median = times[times.len() / 2];
        let variance = times.iter().map(|t| (t - mean).powi(2)).sum::<f64>() / times.len() as f64;
        let std_dev = variance.sqrt();
        let min = times[0];
        let max = times[times.len() - 1];

        Self {
            name,
            iterations: times.len(),
            mean,
            median,
            std_dev,
            min,
            max,
        }
    }

    #[allow(dead_code)]
    fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap()
    }

    fn to_csv_row(&self) -> String {
        format!(
            "{},{},{:.4},{:.4},{:.4},{:.4},{:.4}",
            self.name, self.iterations, self.mean, self.median, self.std_dev, self.min, self.max
        )
    }
}

pub struct BenchCommand {
    #[allow(dead_code)]
    filter: Option<String>,
    #[allow(dead_code)]
    output_format: OutputFormat,
    baseline_file: Option<PathBuf>,
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
enum OutputFormat {
    Text,
    Json,
    Csv,
}

impl BenchCommand {
    pub fn create() -> Box<dyn Command> {
        Box::new(Self {
            filter: None,
            output_format: OutputFormat::Text,
            baseline_file: Some(PathBuf::from(".benchmark_baseline.json")),
        })
    }

    fn discover_benchmarks(&self, project_root: &Path) -> Vec<PathBuf> {
        let mut benchmarks = Vec::new();
        let benches_dir = project_root.join("benches");

        if benches_dir.exists() && benches_dir.is_dir() {
            if let Ok(entries) = std::fs::read_dir(&benches_dir) {
                for entry in entries.flatten() {
                    if let Ok(metadata) = entry.metadata() {
                        if metadata.is_file() {
                            let path = entry.path();
                            if path.extension().and_then(|s| s.to_str()) == Some("kr") {
                                benchmarks.push(path);
                            }
                        }
                    }
                }
            }
        }
        benchmarks
    }

    fn run_benchmark(&self, _bench_path: &Path, iterations: usize) -> Vec<Duration> {
        let mut samples = Vec::new();
        for _ in 0..iterations {
            let start = Instant::now();
            // Simulate benchmark execution
            std::thread::sleep(Duration::from_micros(100));
            samples.push(start.elapsed());
        }
        samples
    }

    fn load_baseline(&self) -> Option<Vec<BenchmarkResults>> {
        if let Some(baseline_path) = &self.baseline_file {
            if baseline_path.exists() {
                if let Ok(content) = fs::read_to_string(baseline_path) {
                    return serde_json::from_str(&content).ok();
                }
            }
        }
        None
    }

    fn save_baseline(&self, results: &[BenchmarkResults]) -> Result<(), String> {
        if let Some(baseline_path) = &self.baseline_file {
            let json = serde_json::to_string_pretty(results)
                .map_err(|e| format!("Failed to serialize results: {e}"))?;
            fs::write(baseline_path, json).map_err(|e| format!("Failed to write baseline: {e}"))?;
        }
        Ok(())
    }

    fn compare_with_baseline(
        &self,
        current: &BenchmarkResults,
        baseline: &[BenchmarkResults],
    ) -> Option<f64> {
        baseline
            .iter()
            .find(|b| b.name == current.name)
            .map(|b| ((current.mean - b.mean) / b.mean) * 100.0)
    }

    fn output_results(&self, results: &[BenchmarkResults], baseline: Option<&[BenchmarkResults]>) {
        match self.output_format {
            OutputFormat::Text => {
                println!("\n{}", "=".repeat(80));
                println!("Benchmark Results");
                println!("{}", "=".repeat(80));

                for result in results {
                    println!("\n{}", result.name);
                    println!("  Iterations: {}", result.iterations);
                    println!("  Mean:       {:.4} ms", result.mean);
                    println!("  Median:     {:.4} ms", result.median);
                    println!("  Std Dev:    {:.4} ms", result.std_dev);
                    println!("  Min:        {:.4} ms", result.min);
                    println!("  Max:        {:.4} ms", result.max);

                    if let Some(base) = baseline {
                        if let Some(change) = self.compare_with_baseline(result, base) {
                            let symbol = if change > 0.0 { "↑" } else { "↓" };
                            let color = if change.abs() < 5.0 {
                                "~"
                            } else if change > 0.0 {
                                "⚠"
                            } else {
                                "✓"
                            };
                            println!(
                                "  Change:     {}{:.2}% {} vs baseline",
                                symbol,
                                change.abs(),
                                color
                            );
                        }
                    }
                }
            }
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(results).unwrap();
                println!("{}", json);
            }
            OutputFormat::Csv => {
                println!("name,iterations,mean_ms,median_ms,std_dev_ms,min_ms,max_ms");
                for result in results {
                    println!("{}", result.to_csv_row());
                }
            }
        }
    }
}

impl Command for BenchCommand {
    fn name(&self) -> &str {
        "bench"
    }

    fn description(&self) -> &str {
        "Run benchmarks with statistical analysis"
    }

    fn execute(&self, args: Vec<String>) -> CommandResult {
        let project_root =
            std::env::current_dir().map_err(|e| format!("Failed to get current directory: {e}"))?;

        let benchmarks = self.discover_benchmarks(&project_root);

        if benchmarks.is_empty() {
            println!(
                "{}",
                OutputMessage::warning("No benchmarks found in benches/")
            );
            return Ok(());
        }

        println!(
            "{}",
            OutputMessage::info(format!("Running {} benchmarks", benchmarks.len()))
        );

        let iterations = 100;
        let progress = ProgressIndicator::new("Benchmarking", benchmarks.len() as u64);
        let mut results = Vec::new();

        for (i, bench_path) in benchmarks.iter().enumerate() {
            progress.update((i + 1) as u64);

            let bench_name = bench_path
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .to_string();
            let samples = self.run_benchmark(bench_path, iterations);
            let result = BenchmarkResults::from_samples(bench_name, &samples);
            results.push(result);
        }

        progress.finish("Benchmarks complete");

        // Load baseline for comparison
        let baseline = self.load_baseline();

        // Output results
        self.output_results(&results, baseline.as_deref());

        // Save as new baseline if requested
        if args.contains(&"--save-baseline".to_string()) {
            self.save_baseline(&results)?;
            println!("{}", OutputMessage::success("Baseline saved"));
        }

        println!("{}", OutputMessage::success("Benchmarking completed"));

        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for BenchCommand {
    fn default() -> Self {
        Self {
            filter: None,
            output_format: OutputFormat::Text,
            baseline_file: Some(PathBuf::from(".benchmark_baseline.json")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bench_command() {
        let cmd = BenchCommand::create();
        assert_eq!(cmd.name(), "bench");
    }

    #[test]
    fn test_benchmark_results() {
        let samples = vec![
            Duration::from_millis(10),
            Duration::from_millis(12),
            Duration::from_millis(11),
        ];
        let result = BenchmarkResults::from_samples("test".to_string(), &samples);
        assert_eq!(result.iterations, 3);
        assert!(result.mean > 10.0 && result.mean < 12.0);
    }

    #[test]
    fn test_csv_output() {
        let result = BenchmarkResults {
            name: "test".to_string(),
            iterations: 100,
            mean: 10.5,
            median: 10.0,
            std_dev: 0.5,
            min: 9.0,
            max: 12.0,
        };
        let csv = result.to_csv_row();
        assert!(csv.contains("test"));
        assert!(csv.contains("100"));
    }
}
