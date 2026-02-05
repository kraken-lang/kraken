//! Format command - Format Kraken source code.

use crate::cli::output::{OutputMessage, ProgressIndicator};
use crate::cli::{Command, CommandResult};
use crate::formatter::Formatter;
use glob::glob;
use std::path::{Path, PathBuf};

pub struct FmtCommand {
    check: bool,
    #[allow(dead_code)]
    config_path: Option<PathBuf>,
}

impl FmtCommand {
    pub fn create() -> Box<dyn Command> {
        Box::new(Self {
            check: false,
            config_path: None,
        })
    }

    fn discover_kraken_files(&self, root: &Path) -> Vec<PathBuf> {
        let mut files = Vec::new();

        // Search for .kr files
        let pattern = format!("{}/**/*.kr", root.display());
        if let Ok(entries) = glob(&pattern) {
            for entry in entries.flatten() {
                files.push(entry);
            }
        }

        files
    }

    fn create_formatter(&self) -> Formatter {
        // Future: load config from file if specified
        Formatter::new()
    }

    fn format_files(&self, files: &[PathBuf]) -> Result<(usize, usize), String> {
        let formatter = self.create_formatter();

        let progress = ProgressIndicator::new("Formatting", files.len() as u64);
        let mut formatted_count = 0;
        let mut unchanged_count = 0;

        for (i, file) in files.iter().enumerate() {
            progress.update((i + 1) as u64);

            if self.check {
                // Check mode - just verify if formatting is needed
                match formatter.check_file(file) {
                    Ok(needs_formatting) => {
                        if needs_formatting {
                            println!(
                                "{}",
                                OutputMessage::warning(format!(
                                    "Needs formatting: {}",
                                    file.display()
                                ))
                            );
                            formatted_count += 1;
                        } else {
                            unchanged_count += 1;
                        }
                    }
                    Err(e) => {
                        println!(
                            "{}",
                            OutputMessage::error(format!(
                                "Error checking {}: {}",
                                file.display(),
                                e
                            ))
                        );
                    }
                }
            } else {
                // Format mode - actually format the files
                match formatter.format_file_in_place(file) {
                    Ok(changed) => {
                        if changed {
                            println!(
                                "{}",
                                OutputMessage::success(format!("Formatted: {}", file.display()))
                            );
                            formatted_count += 1;
                        } else {
                            unchanged_count += 1;
                        }
                    }
                    Err(e) => {
                        println!(
                            "{}",
                            OutputMessage::error(format!(
                                "Error formatting {}: {}",
                                file.display(),
                                e
                            ))
                        );
                    }
                }
            }
        }

        progress.finish("Formatting complete");

        Ok((formatted_count, unchanged_count))
    }
}

impl Command for FmtCommand {
    fn name(&self) -> &str {
        "fmt"
    }

    fn description(&self) -> &str {
        "Format Kraken source code"
    }

    fn execute(&self, args: Vec<String>) -> CommandResult {
        // Parse arguments
        let check_mode = args.contains(&"--check".to_string());

        let project_root =
            std::env::current_dir().map_err(|e| format!("Failed to get current directory: {e}"))?;

        println!("{}", OutputMessage::info("Discovering Kraken source files"));

        let files = self.discover_kraken_files(&project_root);

        if files.is_empty() {
            println!("{}", OutputMessage::warning("No Kraken files found"));
            return Ok(());
        }

        println!(
            "{}",
            OutputMessage::info(format!("Found {} files to format", files.len()))
        );

        let (formatted, unchanged) = self.format_files(&files)?;

        if check_mode {
            if formatted > 0 {
                println!(
                    "{}",
                    OutputMessage::warning(format!(
                        "{} files need formatting, {} files are correctly formatted",
                        formatted, unchanged
                    ))
                );
                return Err(format!("{formatted} files need formatting"));
            } else {
                println!(
                    "{}",
                    OutputMessage::success(format!(
                        "All {} files are correctly formatted",
                        unchanged
                    ))
                );
            }
        } else {
            println!(
                "{}",
                OutputMessage::success(format!(
                    "Formatted {} files, {} files unchanged",
                    formatted, unchanged
                ))
            );
        }

        Ok(())
    }
}

#[allow(clippy::derivable_impls)]
impl Default for FmtCommand {
    fn default() -> Self {
        Self {
            check: false,
            config_path: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fmt_command() {
        let cmd = FmtCommand::create();
        assert_eq!(cmd.name(), "fmt");
        assert!(!cmd.description().is_empty());
    }

    #[test]
    fn test_fmt_default() {
        let cmd = FmtCommand::default();
        assert!(!cmd.check);
        assert!(cmd.config_path.is_none());
    }

    #[test]
    fn test_create_formatter() {
        let cmd = FmtCommand::default();
        let _formatter = cmd.create_formatter();
        // Formatter created successfully
    }
}
