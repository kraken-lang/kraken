//! Kraken CLI - Command-line interface for the Kraken compiler.
//!
//! Custom command framework with rich output formatting, colors, progress indicators,
//! ASCII art, and cross-platform terminal support.

pub mod commands;
/// CLI configuration: color, verbosity, output format, parallelism, and project root.
pub mod config;
/// Rich terminal output: colored messages, ASCII art banners, progress bars, and tables.
pub mod output;

use commands::*;
use config::CliConfig;
use std::collections::HashMap;
use std::env;

/// Result type for CLI command execution (Ok on success, Err with message on failure).
pub type CommandResult = Result<(), String>;

/// Command trait for CLI commands
pub trait Command {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn execute(&self, args: Vec<String>) -> CommandResult;
}

/// Kraken CLI application
pub struct KrakenCli {
    commands: HashMap<String, Box<dyn Command>>,
    #[allow(dead_code)]
    config: CliConfig,
}

impl KrakenCli {
    /// Create a new Kraken CLI instance
    pub fn new() -> Self {
        Self {
            commands: HashMap::new(),
            config: CliConfig::default(),
        }
    }

    /// Register a command
    pub fn add_command(&mut self, command: Box<dyn Command>) {
        self.commands.insert(command.name().to_string(), command);
    }

    /// Register all commands
    pub fn register_commands(&mut self) {
        self.add_command(build::BuildCommand::create());
        self.add_command(run::RunCommand::create());
        self.add_command(test::TestCommand::create());
        self.add_command(check::CheckCommand::create());
        self.add_command(clean::CleanCommand::create());
        self.add_command(init::InitCommand::create());
        self.add_command(new::NewCommand::create());
        self.add_command(version::VersionCommand::create());
        self.add_command(fmt::FmtCommand::create());
        self.add_command(doc::DocCommand::create());
        self.add_command(bench::BenchCommand::create());
    }

    /// Run the CLI with provided arguments
    pub fn run(&self, args: Vec<String>) -> CommandResult {
        if args.len() < 2 {
            self.print_help();
            return Ok(());
        }

        let command_name = &args[1];

        if command_name == "help" || command_name == "--help" || command_name == "-h" {
            self.print_help();
            return Ok(());
        }

        match self.commands.get(command_name) {
            Some(command) => command.execute(args),
            None => Err(format!("Unknown command: {command_name}")),
        }
    }

    /// Run the CLI with system arguments
    pub fn run_with_env_args(&self) -> CommandResult {
        let args: Vec<String> = env::args().collect();
        self.run(args)
    }

    fn print_help(&self) {
        println!("Kraken {}", env!("CARGO_PKG_VERSION"));
        println!("The Kraken programming language compiler and toolchain");
        println!();
        println!("USAGE:");
        println!("    kraken <COMMAND> [OPTIONS]");
        println!();
        println!("COMMANDS:");
        let mut command_names: Vec<_> = self.commands.keys().collect();
        command_names.sort();
        for name in command_names {
            if let Some(cmd) = self.commands.get(name) {
                println!("    {:<12} {}", name, cmd.description());
            }
        }
    }
}

impl Default for KrakenCli {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cli_creation() {
        let cli = KrakenCli::new();
        assert_eq!(cli.commands.len(), 0);
    }

    #[test]
    fn test_cli_version() {
        let mut cli = KrakenCli::new();
        cli.register_commands();
        let result = cli.run(vec!["kraken".to_string(), "version".to_string()]);
        assert!(result.is_ok());
    }

    #[test]
    fn test_cli_help() {
        let mut cli = KrakenCli::new();
        cli.register_commands();
        let result = cli.run(vec!["kraken".to_string(), "help".to_string()]);
        assert!(result.is_ok());
    }

    #[test]
    fn test_cli_unknown_command() {
        let mut cli = KrakenCli::new();
        cli.register_commands();
        let result = cli.run(vec!["kraken".to_string(), "unknown".to_string()]);
        assert!(result.is_err());
    }
}
