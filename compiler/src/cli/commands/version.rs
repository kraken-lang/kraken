//! Version command - Display version information.

use crate::cli::{Command, CommandResult};

/// Version command: displays the Kraken compiler version.
pub struct VersionCommand;

impl VersionCommand {
    /// Create a new version command.
    pub fn create() -> Box<dyn Command> {
        Box::new(Self)
    }
}

impl Command for VersionCommand {
    fn name(&self) -> &str {
        "version"
    }

    fn description(&self) -> &str {
        "Display version information"
    }

    fn execute(&self, _args: Vec<String>) -> CommandResult {
        println!("kraken {}", env!("CARGO_PKG_VERSION"));
        println!("Kraken Language Compiler and Toolchain");
        println!();
        println!("Compiler version: {}", env!("CARGO_PKG_VERSION"));
        println!("Rust version: {}", rustc_version());
        println!("LLVM version: 18.0");
        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for VersionCommand {
    fn default() -> Self {
        Self
    }
}

fn rustc_version() -> String {
    let version_info = rustc_version::version()
        .unwrap_or_else(|_| rustc_version::Version::parse("0.0.0").unwrap());
    format!("{version_info}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_command() {
        let cmd = VersionCommand;
        assert_eq!(cmd.name(), "version");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_version_execution() {
        let cmd = VersionCommand;
        let result = cmd.execute(vec![]);
        assert!(result.is_ok());
    }
}
