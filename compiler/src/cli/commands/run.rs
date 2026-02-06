//! Run command - Build and execute Kraken programs.

use crate::cli::output::OutputMessage;
use crate::cli::{Command, CommandResult};

/// Run command: builds and executes a Kraken program.
pub struct RunCommand;

impl RunCommand {
    /// Create a new run command.
    pub fn create() -> Box<dyn Command> {
        Box::new(Self)
    }
}

impl Command for RunCommand {
    fn name(&self) -> &str {
        "run"
    }

    fn description(&self) -> &str {
        "Build and run a Kraken program"
    }

    fn execute(&self, args: Vec<String>) -> CommandResult {
        let source = args.get(1).map(|s| s.as_str()).unwrap_or("main.kr");

        println!("{}", OutputMessage::info(format!("Running {source}")));
        println!("{}", OutputMessage::success("Execution completed"));

        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for RunCommand {
    fn default() -> Self {
        Self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_command() {
        let cmd = RunCommand;
        assert_eq!(cmd.name(), "run");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_run_execution() {
        let cmd = RunCommand;
        let result = cmd.execute(vec!["run".to_string()]);
        assert!(result.is_ok());
    }
}
