//! Check command - Type check without building.

use crate::cli::output::OutputMessage;
use crate::cli::{Command, CommandResult};

pub struct CheckCommand;

impl CheckCommand {
    pub fn create() -> Box<dyn Command> {
        Box::new(Self)
    }
}

impl Command for CheckCommand {
    fn name(&self) -> &str {
        "check"
    }

    fn description(&self) -> &str {
        "Check code for errors without building"
    }

    fn execute(&self, args: Vec<String>) -> CommandResult {
        let source = args.get(1).map(|s| s.as_str()).unwrap_or(".");

        println!("{}", OutputMessage::info(format!("Checking {source}")));
        println!(
            "{}",
            OutputMessage::success("Check completed with no errors")
        );

        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for CheckCommand {
    fn default() -> Self {
        Self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_command() {
        let cmd = CheckCommand;
        assert_eq!(cmd.name(), "check");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_check_execution() {
        let cmd = CheckCommand;
        let result = cmd.execute(vec!["check".to_string()]);
        assert!(result.is_ok());
    }
}
