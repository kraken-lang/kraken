//! Init command - Initialize a new Kraken project in existing directory.

use crate::cli::output::OutputMessage;
use crate::cli::{Command, CommandResult};
use std::fs;
use std::path::Path;

/// Init command: initializes a new Kraken project in the current directory.
pub struct InitCommand;

impl InitCommand {
    /// Create a new init command.
    pub fn create() -> Box<dyn Command> {
        Box::new(Self)
    }

    fn create_project_structure(&self, name: &str) -> Result<(), String> {
        println!(
            "{}",
            OutputMessage::info(format!("Initializing project '{name}'"))
        );

        fs::create_dir_all("src").map_err(|e| format!("Failed to create src directory: {e}"))?;

        let main_content = r#"fn main() -> int {
    puts("Hello, Kraken!");
    return 0;
}
"#;
        fs::write("src/main.kr", main_content)
            .map_err(|e| format!("Failed to create main.kr: {e}"))?;

        let kraken_toml = format!(
            r#"[package]
name = "{name}"
version = "0.1.0"
edition = "2024"

[dependencies]
"#
        );
        fs::write("Kraken.toml", kraken_toml)
            .map_err(|e| format!("Failed to create Kraken.toml: {e}"))?;

        let gitignore = r#"/target
/build
*.o
*.so
*.dylib
*.dll
*.exe
"#;
        fs::write(".gitignore", gitignore)
            .map_err(|e| format!("Failed to create .gitignore: {e}"))?;

        println!(
            "{}",
            OutputMessage::success("Project initialized successfully")
        );
        println!("  Created src/main.kr");
        println!("  Created Kraken.toml");
        println!("  Created .gitignore");

        Ok(())
    }
}

impl Command for InitCommand {
    fn name(&self) -> &str {
        "init"
    }

    fn description(&self) -> &str {
        "Initialize a new Kraken project in the current directory"
    }

    fn execute(&self, _args: Vec<String>) -> CommandResult {
        let current_dir =
            std::env::current_dir().map_err(|e| format!("Failed to get current directory: {e}"))?;

        let project_name = current_dir
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("kraken-project");

        if Path::new("Kraken.toml").exists() {
            return Err("Kraken.toml already exists in current directory".to_string());
        }

        self.create_project_structure(project_name)?;

        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for InitCommand {
    fn default() -> Self {
        Self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init_command() {
        let cmd = InitCommand;
        assert_eq!(cmd.name(), "init");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_init_create() {
        let cmd = InitCommand::create();
        assert_eq!(cmd.name(), "init");
    }

    #[test]
    fn test_init_default() {
        let _cmd = InitCommand::default();
    }
}
