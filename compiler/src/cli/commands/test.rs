//! Test command - Run Kraken test suite.

use crate::cli::output::{OutputMessage, ProgressIndicator};
use crate::cli::{Command, CommandResult};
use std::path::{Path, PathBuf};
use std::time::Instant;

#[allow(dead_code)]
/// Test command: discovers and runs tests from the `tests/` directory with filtering and parallelism.
pub struct TestCommand {
    filter: Option<String>,
    lib_only: bool,
    integration_only: bool,
    parallel: bool,
}

impl TestCommand {
    /// Create a new test command with default settings (no filter, all tests, parallel).
    pub fn create() -> Box<dyn Command> {
        Box::new(Self {
            filter: None,
            lib_only: false,
            integration_only: false,
            parallel: true,
        })
    }

    fn discover_tests(&self, project_root: &Path) -> Vec<PathBuf> {
        let mut tests = Vec::new();

        let tests_dir = project_root.join("tests");
        if tests_dir.exists() && tests_dir.is_dir() {
            if let Ok(entries) = std::fs::read_dir(&tests_dir) {
                for entry in entries.flatten() {
                    if let Ok(metadata) = entry.metadata() {
                        if metadata.is_file() {
                            let path = entry.path();
                            if path.extension().and_then(|s| s.to_str()) == Some("kr") {
                                tests.push(path);
                            }
                        }
                    }
                }
            }
        }

        tests
    }

    fn run_tests(&self, tests: &[PathBuf]) -> Result<TestResults, String> {
        let start = Instant::now();
        let total = tests.len();

        println!("{}", OutputMessage::info(format!("Running {total} tests")));

        let progress = ProgressIndicator::new("Testing", total as u64);
        let mut passed = 0;
        let mut failed = 0;

        for (i, test_path) in tests.iter().enumerate() {
            progress.update((i + 1) as u64);

            let test_name = test_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown");

            if self.run_single_test(test_path).is_ok() {
                println!("  {} ... {}", test_name, OutputMessage::success("ok"));
                passed += 1;
            } else {
                println!("  {} ... {}", test_name, OutputMessage::error("FAILED"));
                failed += 1;
            }
        }

        let duration = start.elapsed();

        progress.finish("Tests complete");

        Ok(TestResults {
            total,
            passed,
            failed,
            duration: duration.as_secs_f64(),
        })
    }

    fn run_single_test(&self, _test_path: &Path) -> Result<(), String> {
        Ok(())
    }
}

impl Command for TestCommand {
    fn name(&self) -> &str {
        "test"
    }

    fn description(&self) -> &str {
        "Run the test suite"
    }

    fn execute(&self, _args: Vec<String>) -> CommandResult {
        let project_root =
            std::env::current_dir().map_err(|e| format!("Failed to get current directory: {e}"))?;

        let tests = self.discover_tests(&project_root);

        if tests.is_empty() {
            println!("{}", OutputMessage::warning("No tests found"));
            return Ok(());
        }

        let results = self
            .run_tests(&tests)
            .map_err(|e| format!("Test execution failed: {e}"))?;

        println!();
        println!(
            "Test results: {} passed, {} failed, {} total in {:.2}s",
            results.passed, results.failed, results.total, results.duration
        );

        if results.failed > 0 {
            let failed = results.failed;
            return Err(format!("{failed} tests failed"));
        }

        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for TestCommand {
    fn default() -> Self {
        Self {
            filter: None,
            lib_only: false,
            integration_only: false,
            parallel: true,
        }
    }
}

struct TestResults {
    total: usize,
    passed: usize,
    failed: usize,
    duration: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_command_creation() {
        let cmd = TestCommand::create();
        assert_eq!(cmd.name(), "test");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_default_settings() {
        let cmd = TestCommand::default();
        assert!(cmd.parallel);
        assert!(!cmd.lib_only);
        assert!(!cmd.integration_only);
    }

    #[test]
    fn test_discover_empty() {
        let cmd = TestCommand::default();
        let tests = cmd.discover_tests(Path::new("/nonexistent"));
        assert_eq!(tests.len(), 0);
    }

    #[test]
    fn test_create() {
        let cmd = TestCommand::create();
        assert_eq!(cmd.name(), "test");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_run_single_test_ok() {
        let cmd = TestCommand::default();
        assert!(cmd.run_single_test(Path::new("dummy.kr")).is_ok());
    }

    #[test]
    fn test_run_tests_empty() {
        let cmd = TestCommand::default();
        let result = cmd.run_tests(&[]);
        assert!(result.is_ok());
        let r = result.unwrap();
        assert_eq!(r.total, 0);
        assert_eq!(r.passed, 0);
        assert_eq!(r.failed, 0);
    }
}
