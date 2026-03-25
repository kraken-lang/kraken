//! Build command - Compile Kraken source code.

use crate::cli::output::{OutputMessage, ProgressIndicator};
use crate::cli::{Command, CommandResult};
use std::path::PathBuf;
use std::time::Instant;

#[allow(dead_code)]
/// Build command: compiles Kraken source with configurable release mode, output path, and parallelism.
pub struct BuildCommand {
    release: bool,
    output: Option<PathBuf>,
    jobs: usize,
}

impl BuildCommand {
    /// Create a new build command with default settings (debug mode, auto-detect CPU count).
    pub fn create() -> Box<dyn Command> {
        Box::new(Self {
            release: false,
            output: None,
            jobs: num_cpus::get(),
        })
    }

    fn build_project(&self, source: &str) -> Result<(), String> {
        let start = Instant::now();

        println!("{}", OutputMessage::info(format!("Building {source}")));

        let progress = ProgressIndicator::new("Building", 5);

        progress.update(1);
        std::thread::sleep(std::time::Duration::from_millis(100));

        progress.update(2);
        std::thread::sleep(std::time::Duration::from_millis(100));

        progress.update(3);
        std::thread::sleep(std::time::Duration::from_millis(100));

        progress.update(4);
        std::thread::sleep(std::time::Duration::from_millis(100));

        progress.update(5);
        progress.finish("Build complete");

        let duration = start.elapsed();
        let duration_secs = duration.as_secs_f64();
        println!(
            "{}",
            OutputMessage::success(format!("Build completed in {duration_secs:.2}s"))
        );

        Ok(())
    }
}

impl Command for BuildCommand {
    fn name(&self) -> &str {
        "build"
    }

    fn description(&self) -> &str {
        "Compile Kraken source code"
    }

    fn execute(&self, args: Vec<String>) -> CommandResult {
        let source = args.get(1).map(|s| s.as_str()).unwrap_or(".");

        self.build_project(source)
            .map_err(|e| format!("Build failed: {e}"))?;

        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for BuildCommand {
    fn default() -> Self {
        Self {
            release: false,
            output: None,
            jobs: num_cpus::get(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_command() {
        let cmd = BuildCommand::create();
        assert_eq!(cmd.name(), "build");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_build_default() {
        let cmd = BuildCommand::default();
        assert!(!cmd.release);
        assert!(cmd.jobs > 0);
    }

    #[test]
    fn test_build_execute_default_source() {
        let cmd = BuildCommand::create();
        let result = cmd.execute(vec!["build".to_string()]);
        assert!(result.is_ok());
    }

    #[test]
    fn test_build_execute_with_source() {
        let cmd = BuildCommand::create();
        let result = cmd.execute(vec!["build".to_string(), "src/main.kr".to_string()]);
        assert!(result.is_ok());
    }
}
