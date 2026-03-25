//! New command - Create a new Kraken project.

use crate::cli::output::OutputMessage;
use crate::cli::{Command, CommandResult};
use std::fs;
use std::path::Path;

/// New command: creates a new Kraken project directory from a template.
pub struct NewCommand;

impl NewCommand {
    /// Create a new project creation command.
    pub fn create() -> Box<dyn Command> {
        Box::new(Self)
    }

    fn create_project(&self, name: &str) -> Result<(), String> {
        println!(
            "{}",
            OutputMessage::info(format!("Creating new project '{name}'"))
        );

        if Path::new(name).exists() {
            return Err(format!("Directory '{name}' already exists"));
        }

        fs::create_dir(name).map_err(|e| format!("Failed to create project directory: {e}"))?;

        let src_dir = Path::new(name).join("src");
        fs::create_dir(&src_dir).map_err(|e| format!("Failed to create src directory: {e}"))?;

        let main_content = r#"fn main() -> int {
    puts("Hello, Kraken!");
    return 0;
}
"#;
        fs::write(src_dir.join("main.kr"), main_content)
            .map_err(|e| format!("Failed to create main.kr: {e}"))?;

        let kraken_toml = format!(
            r#"[package]
name = "{name}"
version = "0.1.0"
edition = "2024"

[dependencies]
"#
        );
        fs::write(Path::new(name).join("Kraken.toml"), kraken_toml)
            .map_err(|e| format!("Failed to create Kraken.toml: {e}"))?;

        let gitignore = r#"/target
/build
*.o
*.so
*.dylib
*.dll
*.exe
"#;
        fs::write(Path::new(name).join(".gitignore"), gitignore)
            .map_err(|e| format!("Failed to create .gitignore: {e}"))?;

        let readme = format!(
            r#"# {name}

A Kraken language project.

## Building

```bash
kraken build
```

## Running

```bash
kraken run
```
"#
        );
        fs::write(Path::new(name).join("README.md"), readme)
            .map_err(|e| format!("Failed to create README.md: {e}"))?;

        println!(
            "{}",
            OutputMessage::success(format!("Created project '{name}'"))
        );
        println!("  Created {name}/src/main.kr");
        println!("  Created {name}/Kraken.toml");
        println!("  Created {name}/.gitignore");
        println!("  Created {name}/README.md");

        Ok(())
    }
}

impl Command for NewCommand {
    fn name(&self) -> &str {
        "new"
    }

    fn description(&self) -> &str {
        "Create a new Kraken project"
    }

    fn execute(&self, args: Vec<String>) -> CommandResult {
        let project_name = args.get(1).ok_or("Project name required")?;

        self.create_project(project_name)?;

        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for NewCommand {
    fn default() -> Self {
        Self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_command() {
        let cmd = NewCommand;
        assert_eq!(cmd.name(), "new");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_new_requires_name() {
        let cmd = NewCommand;
        let result = cmd.execute(vec!["new".to_string()]);
        assert!(result.is_err());
    }

    #[test]
    fn test_new_create() {
        let cmd = NewCommand::create();
        assert_eq!(cmd.name(), "new");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_new_default() {
        let _cmd = NewCommand::default();
    }

    #[test]
    fn test_new_existing_dir_error() {
        let tmp = std::env::temp_dir();
        let existing = tmp.join("kraken_test_new_existing");
        let _ = std::fs::create_dir(&existing);
        let cmd = NewCommand;
        let result = cmd.create_project(existing.to_str().unwrap());
        assert!(result.is_err());
        let _ = std::fs::remove_dir_all(&existing);
    }

    #[test]
    fn test_new_creates_project() {
        let tmp = std::env::temp_dir();
        let name = format!("kraken_test_new_{}", std::process::id());
        let project_dir = tmp.join(&name);
        let _ = std::fs::remove_dir_all(&project_dir);

        let cmd = NewCommand;
        let saved = std::env::current_dir().unwrap();
        std::env::set_current_dir(&tmp).unwrap();
        let result = cmd.create_project(&name);
        std::env::set_current_dir(&saved).unwrap();

        assert!(result.is_ok());
        assert!(project_dir.join("src/main.kr").exists());
        assert!(project_dir.join("Kraken.toml").exists());
        assert!(project_dir.join(".gitignore").exists());
        assert!(project_dir.join("README.md").exists());
        let _ = std::fs::remove_dir_all(&project_dir);
    }
}
