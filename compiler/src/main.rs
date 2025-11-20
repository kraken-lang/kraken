//! Kraken Compiler CLI
//! 
//! High-performance systems programming language compiler.

use anyhow::{Context, Result};
use clap::{Parser as ClapParser, Subcommand};
use std::path::PathBuf;
use tokio::fs;

mod error;
mod lexer;
mod parser;
mod analyzer;
mod codegen;

use lexer::tokenizer::{Tokenizer, is_kraken_source_file};
use parser::Parser;
use analyzer::TypeChecker;
use codegen::LLVMCodegen;

/// Kraken Programming Language Compiler
#[derive(ClapParser, Debug)]
#[command(name = "kraken")]
#[command(about = "Kraken programming language compiler", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Build a Kraken project
    Build {
        /// Source file or directory to compile
        #[arg(value_name = "PATH")]
        path: PathBuf,

        /// Output file path
        #[arg(short, long, value_name = "FILE")]
        output: Option<PathBuf>,

        /// Enable verbose output
        #[arg(short, long)]
        verbose: bool,
    },

    /// Run a Kraken program
    Run {
        /// Source file to run
        #[arg(value_name = "FILE")]
        file: PathBuf,

        /// Arguments to pass to the program
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },

    /// Check a Kraken program for errors without building
    Check {
        /// Source file or directory to check
        #[arg(value_name = "PATH")]
        path: PathBuf,

        /// Enable verbose output
        #[arg(short, long)]
        verbose: bool,
    },

    /// Create a new Kraken project
    New {
        /// Project name
        #[arg(value_name = "NAME")]
        name: String,

        /// Project directory (defaults to current directory)
        #[arg(short, long, value_name = "DIR")]
        path: Option<PathBuf>,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build { path, output, verbose } => {
            build_command(path, output, verbose).await?;
        }
        Commands::Run { file, args } => {
            run_command(file, args).await?;
        }
        Commands::Check { path, verbose } => {
            check_command(path, verbose).await?;
        }
        Commands::New { name, path } => {
            new_command(name, path).await?;
        }
    }

    Ok(())
}

/// Build command implementation.
async fn build_command(path: PathBuf, output: Option<PathBuf>, verbose: bool) -> Result<()> {
    if verbose {
        println!("Building Kraken project at: {}", path.display());
    }

    let files = discover_source_files(&path)?;

    if files.is_empty() {
        anyhow::bail!("No Kraken source files found in {}", path.display());
    }

    if verbose {
        println!("Found {} source file(s)", files.len());
    }

    for file in &files {
        if verbose {
            println!("Compiling: {}", file.display());
        }

        compile_file(file).await.context(format!("Failed to compile {}", file.display()))?;
    }

    let output_path = output.unwrap_or_else(|| PathBuf::from("output"));
    
    if verbose {
        println!("Build successful! Output: {}", output_path.display());
    } else {
        println!("Build complete.");
    }

    Ok(())
}

/// Run command implementation.
async fn run_command(file: PathBuf, _args: Vec<String>) -> Result<()> {
    if !is_kraken_source_file(&file) {
        anyhow::bail!("File must have .kr or .krak extension");
    }

    println!("Compiling and running: {}", file.display());

    compile_file(&file).await?;

    println!("Execution complete.");
    Ok(())
}

/// Check command implementation.
async fn check_command(path: PathBuf, verbose: bool) -> Result<()> {
    if verbose {
        println!("Checking Kraken project at: {}", path.display());
    }

    let files = discover_source_files(&path)?;

    if files.is_empty() {
        anyhow::bail!("No Kraken source files found in {}", path.display());
    }

    let mut errors = Vec::new();

    for file in &files {
        if verbose {
            println!("Checking: {}", file.display());
        }

        if let Err(e) = check_file(file).await {
            errors.push((file.clone(), e));
        }
    }

    if errors.is_empty() {
        println!("All checks passed!");
        Ok(())
    } else {
        eprintln!("Found {} error(s):", errors.len());
        for (file, error) in errors {
            eprintln!("  {}: {}", file.display(), error);
        }
        anyhow::bail!("Check failed");
    }
}

/// New project command implementation.
async fn new_command(name: String, path: Option<PathBuf>) -> Result<()> {
    let project_dir = path.unwrap_or_else(|| PathBuf::from(&name));

    if project_dir.exists() {
        anyhow::bail!("Directory already exists: {}", project_dir.display());
    }

    fs::create_dir_all(&project_dir).await?;
    fs::create_dir_all(project_dir.join("src")).await?;

    let main_kr = r#"fn main() -> int {
    println("Hello, Kraken!");
    return 0;
}
"#;

    fs::write(project_dir.join("src/main.kr"), main_kr).await?;

    let kraken_toml = format!(
        r#"[package]
name = "{name}"
version = "0.1.0"

[dependencies]
"#
    );

    fs::write(project_dir.join("Kraken.toml"), kraken_toml).await?;

    println!("Created new Kraken project: {name}");
    println!("  Directory: {}", project_dir.display());

    Ok(())
}

/// Compile a single source file to executable.
async fn compile_file(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file)
        .await
        .context("Failed to read source file")?;

    let mut tokenizer = Tokenizer::new(source, file.clone());
    let tokens = tokenizer.tokenize().context("Lexer error")?;

    let mut parser = Parser::new(tokens, file.clone());
    let program = parser.parse().context("Parser error")?;

    let mut type_checker = TypeChecker::new(file.clone());
    type_checker.check_program(&program).context("Type checking error")?;

    let module_name = file.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module")
        .to_string();

    // Generate object file
    let mut codegen = LLVMCodegen::new(module_name.clone(), file.clone());
    let object_file = file.with_extension("o");
    codegen.compile(&program, &object_file).context("Code generation error")?;

    // Link to executable
    let executable = file.with_extension("");
    link_executable(&object_file, &executable)?;

    // Clean up object file
    std::fs::remove_file(&object_file).ok();

    Ok(())
}

/// Link object file to executable.
fn link_executable(object_file: &PathBuf, output: &PathBuf) -> Result<()> {
    let status = std::process::Command::new("clang")
        .arg(object_file)
        .arg("-o")
        .arg(output)
        .status()
        .context("Failed to run linker")?;

    if !status.success() {
        anyhow::bail!("Linking failed");
    }

    Ok(())
}

/// Check a single source file without generating code.
async fn check_file(file: &PathBuf) -> Result<()> {
    let source = fs::read_to_string(file)
        .await
        .context("Failed to read source file")?;

    let mut tokenizer = Tokenizer::new(source, file.clone());
    let tokens = tokenizer.tokenize().context("Lexer error")?;

    let mut parser = Parser::new(tokens, file.clone());
    let program = parser.parse().context("Parser error")?;

    let mut type_checker = TypeChecker::new(file.clone());
    type_checker.check_program(&program).context("Type checking error")?;

    Ok(())
}

/// Discover Kraken source files in a directory.
fn discover_source_files(path: &PathBuf) -> Result<Vec<PathBuf>> {
    use std::fs;
    
    let mut files = Vec::new();

    if path.is_file() {
        if is_kraken_source_file(path) {
            files.push(path.clone());
        }
    } else if path.is_dir() {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let entry_path = entry.path();

            if entry_path.is_file() && is_kraken_source_file(&entry_path) {
                files.push(entry_path);
            } else if entry_path.is_dir() {
                let sub_files = discover_source_files(&entry_path)?;
                files.extend(sub_files);
            }
        }
    }

    Ok(files)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_kraken_file() {
        assert!(is_kraken_source_file(&PathBuf::from("test.kr")));
        assert!(is_kraken_source_file(&PathBuf::from("test.krak")));
        assert!(!is_kraken_source_file(&PathBuf::from("test.rs")));
    }
}
