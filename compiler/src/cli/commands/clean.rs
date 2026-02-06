//! Clean command - Remove build artifacts.

use crate::cli::output::OutputMessage;
use crate::cli::{Command, CommandResult};
use std::path::Path;

/// Clean command: removes build artifacts from the output directory.
pub struct CleanCommand;

impl CleanCommand {
    /// Create a new clean command.
    pub fn create() -> Box<dyn Command> {
        Box::new(Self)
    }

    fn clean_directory(&self, dir: &Path) -> Result<(), String> {
        if dir.exists() {
            std::fs::remove_dir_all(dir).map_err(|e| format!("Failed to remove directory: {e}"))?;
            let dir_path = dir.display();
            println!("{}", OutputMessage::success(format!("Removed {dir_path}")));
        }
        Ok(())
    }
}

impl Command for CleanCommand {
    fn name(&self) -> &str {
        "clean"
    }

    fn description(&self) -> &str {
        "Remove build artifacts"
    }

    fn execute(&self, _args: Vec<String>) -> CommandResult {
        println!("{}", OutputMessage::info("Cleaning build artifacts"));

        let target_dir = Path::new("target");
        self.clean_directory(target_dir)?;

        let build_dir = Path::new("build");
        self.clean_directory(build_dir)?;

        println!("{}", OutputMessage::success("Clean completed"));

        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for CleanCommand {
    fn default() -> Self {
        Self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clean_command() {
        let cmd = CleanCommand;
        assert_eq!(cmd.name(), "clean");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_clean_nonexistent() {
        let cmd = CleanCommand;
        let result = cmd.clean_directory(Path::new("/nonexistent/dir"));
        assert!(result.is_ok());
    }
}
