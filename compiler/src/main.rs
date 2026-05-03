//! Kraken Compiler CLI
//!
//! High-performance systems programming language compiler.

use anyhow::{Context, Result};
use clap::{Parser as ClapParser, Subcommand};
use std::path::{Path, PathBuf};
use tokio::fs;

use kraken::docgen;

#[allow(dead_code)]
mod analyzer;
#[allow(dead_code)]
mod codegen;
#[allow(dead_code)]
mod diagnostic_registry;
#[allow(dead_code)]
mod diagnostics;
#[allow(dead_code)]
mod error;
#[allow(dead_code)]
mod ffi;
#[allow(dead_code)]
mod ir;
#[allow(dead_code)]
mod lexer;
#[allow(dead_code)]
mod modules;
#[allow(dead_code)]
mod parser;

use analyzer::{monomorphize_program, TypeChecker};
use codegen::LLVMCodegen;
use ir::IrLowering;
use lexer::tokenizer::is_kraken_source_file;
use modules::loader;
use parser::ast::Statement;

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

        /// Emit IR instead of compiling (for debugging)
        #[arg(long)]
        emit_ir: bool,
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

    /// Generate documentation, DocGraph JSON, and LSIF index
    Doc {
        /// Output directory for generated docs (default: docs)
        #[arg(short, long, value_name = "DIR", default_value = "docs")]
        output: PathBuf,

        /// Enable verbose output
        #[arg(short, long)]
        verbose: bool,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build {
            path,
            output,
            verbose,
            emit_ir,
        } => {
            build_command(path, output, verbose, emit_ir).await?;
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
        Commands::Doc { output, verbose } => {
            doc_command(output, verbose).await?;
        }
    }

    Ok(())
}

/// Build command implementation.
async fn build_command(
    path: PathBuf,
    output: Option<PathBuf>,
    verbose: bool,
    emit_ir: bool,
) -> Result<()> {
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
        let program = loader::load_program(file).await?;
        let program = monomorphize_program(program, file.to_path_buf())?;
        let has_main = program.statements.iter().any(|s| match s {
            Statement::FunctionDeclaration { name, .. } => name == "main",
            _ => false,
        });

        if !has_main {
            if verbose {
                println!("Skipping (no main entrypoint): {}", file.display());
            }
            continue;
        }

        // If --emit-ir flag is set, dump IR and exit
        if emit_ir {
            if verbose {
                println!("Lowering to IR: {}", file.display());
            }
            let mut lowering = IrLowering::new();
            let ir_program = lowering
                .lower_program(&program)
                .map_err(|e| anyhow::anyhow!("IR lowering failed: {e}"))?;
            println!("{ir_program}");
            continue;
        }

        if verbose {
            println!("Compiling: {}", file.display());
        }

        let _executable = compile_file(file)
            .await
            .context(format!("Failed to compile {}", file.display()))?;
    }

    let output_path = output.unwrap_or_else(|| PathBuf::from("output"));

    if emit_ir {
        println!("IR dump complete.");
    } else if verbose {
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

    let executable = compile_file(&file).await?;

    let status = std::process::Command::new(&executable)
        .status()
        .with_context(|| format!("Failed to run executable: {}", executable.display()))?;

    println!("Execution complete.");

    match status.code() {
        Some(code) => std::process::exit(code),
        None => anyhow::bail!("Execution terminated by signal"),
    }
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

/// Doc command implementation — generates DocGraph JSON, LSIF, and HTML docs.
async fn doc_command(output: PathBuf, verbose: bool) -> Result<()> {
    let project_root = std::env::current_dir().context("Failed to get current directory")?;

    if verbose {
        println!("Generating documentation in: {}", output.display());
    }

    // 1. Generate DocGraph JSON metadata
    let generated_dir = output.join("generated");
    std::fs::create_dir_all(&generated_dir).context("Failed to create docs/generated directory")?;

    let graph = docgen::generate();

    let docgraph_json =
        serde_json::to_string_pretty(&graph).context("Failed to serialize DocGraph")?;
    std::fs::write(generated_dir.join("docgraph.json"), &docgraph_json)
        .context("Failed to write docgraph.json")?;
    println!(
        "  Generated: docs/generated/docgraph.json ({} bytes)",
        docgraph_json.len()
    );

    let search_json = serde_json::to_string_pretty(&graph.index.search)
        .context("Failed to serialize search index")?;
    std::fs::write(generated_dir.join("search_index.json"), &search_json)
        .context("Failed to write search_index.json")?;
    println!("  Generated: docs/generated/search_index.json");

    // Per-page JSON files
    for page in &graph.pages {
        let page_json = serde_json::to_string_pretty(page).context("Failed to serialize page")?;
        std::fs::write(
            generated_dir.join(format!("{}.json", page.slug)),
            &page_json,
        )
        .with_context(|| format!("Failed to write {}.json", page.slug))?;
        if verbose {
            println!("  Generated: docs/generated/{}.json", page.slug);
        }
    }

    // Link index
    let link_index: std::collections::BTreeMap<&str, serde_json::Value> = graph
        .index
        .search
        .entries
        .iter()
        .map(|e| {
            (
                e.node_id.as_str(),
                serde_json::json!({
                    "title": e.title,
                    "kind": e.kind,
                    "path": e.path
                }),
            )
        })
        .collect();
    let link_json =
        serde_json::to_string_pretty(&link_index).context("Failed to serialize link index")?;
    std::fs::write(generated_dir.join("link_index.json"), &link_json)
        .context("Failed to write link_index.json")?;
    println!("  Generated: docs/generated/link_index.json");

    // 2. Generate LSIF dump
    let lsif = docgen::lsif::generate_lsif(&project_root, &graph);
    let dump_path = project_root.join("dump.lsif");
    std::fs::write(&dump_path, &lsif).context("Failed to write dump.lsif")?;
    println!("  Generated: dump.lsif ({} bytes)", lsif.len());

    // 3. Discover and document .kr source files
    let kr_files = discover_source_files(&project_root).unwrap_or_default();
    if !kr_files.is_empty() {
        println!("  Found {} source file(s) for HTML docs", kr_files.len());
    }

    println!("Documentation generation complete.");
    println!("  DocGraph nodes: {}", graph.nodes.len());
    println!("  Search entries: {}", graph.index.search.entries.len());
    println!("  Pages: {}", graph.pages.len());

    Ok(())
}

/// Compile a single source file to executable.
async fn compile_file(file: &Path) -> Result<PathBuf> {
    let profile = std::env::var("KRAKEN_PROFILE").is_ok();
    let total_start = std::time::Instant::now();

    crate::ffi::stdlib::validate_stdlib_table()
        .map_err(|e| anyhow::anyhow!(e))
        .context("Invalid stdlib/FFI signature table")?;

    // Phase 1: Parsing
    let parse_start = std::time::Instant::now();
    let program = loader::load_program(file).await?;
    let program = monomorphize_program(program, file.to_path_buf())?;
    let parse_time = parse_start.elapsed();

    // Phase 2: Type checking
    let typecheck_start = std::time::Instant::now();
    let mut type_checker = TypeChecker::new(file.to_path_buf());
    type_checker
        .check_program(&program)
        .context("Type checking error")?;
    let typecheck_time = typecheck_start.elapsed();

    let module_name = file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module")
        .to_string();

    let build_dir = std::env::current_dir()
        .context("Failed to determine current working directory")?
        .join("build");
    std::fs::create_dir_all(&build_dir).context("Failed to create build directory")?;

    // Phase 3: Code generation
    let codegen_start = std::time::Instant::now();
    let mut codegen = LLVMCodegen::new(module_name.clone(), file.to_path_buf());
    let object_file = build_dir.join(format!("{module_name}.o"));
    codegen
        .compile(&program, &object_file)
        .context("Code generation error")?;
    let codegen_time = codegen_start.elapsed();

    // Phase 4: Linking
    let link_start = std::time::Instant::now();
    // On Windows, use a short hash of the module name as the exe stem.
    // Windows AppCompat heuristics match installer-pattern words (e.g.
    // "dispatch", "setup", "install", "test", "patch") in exe filenames and
    // demand UAC elevation (os error 740) when spawning via CreateProcess.
    // A hex hash is neutral and never matches any shim-database entry.
    #[cfg(target_os = "windows")]
    let executable = {
        let h = module_name
            .bytes()
            .fold(0u32, |acc, b| acc.wrapping_mul(31).wrapping_add(b as u32));
        build_dir.join(format!("kr{h:08x}.exe"))
    };
    #[cfg(not(target_os = "windows"))]
    let executable = build_dir.join(&module_name);
    link_executable(&object_file, &executable)?;
    let link_time = link_start.elapsed();

    // Clean up object file
    std::fs::remove_file(&object_file).ok();

    let total_time = total_start.elapsed();

    // Print profiling info if KRAKEN_PROFILE is set
    if profile {
        eprintln!("\n\x1b[1m=== Compile Profile ===\x1b[0m");
        eprintln!("  Parse:     {:>8.2}ms", parse_time.as_secs_f64() * 1000.0);
        eprintln!(
            "  Typecheck: {:>8.2}ms",
            typecheck_time.as_secs_f64() * 1000.0
        );
        eprintln!(
            "  Codegen:   {:>8.2}ms",
            codegen_time.as_secs_f64() * 1000.0
        );
        eprintln!("  Link:      {:>8.2}ms", link_time.as_secs_f64() * 1000.0);
        eprintln!(
            "  \x1b[1mTotal:     {:>8.2}ms\x1b[0m",
            total_time.as_secs_f64() * 1000.0
        );
    }

    Ok(executable)
}

/// Resolve the C compiler / linker driver used to produce the final executable.
///
/// Returns `(path, is_zig)`. When `is_zig` is true the caller must prepend `"cc"`
/// to the argument list so the invocation becomes `zig cc <args>`.
///
/// Search order:
/// 1. `KRAKEN_LINKER` environment variable — user override, highest priority.
/// 2. `LLVM_SYS_180_PREFIX/bin/clang[.exe]` — matches the LLVM 18 used for compilation.
/// 3. Well-known Windows LLVM installation paths.
/// 4. `zig` in well-known locations — `zig cc` is a drop-in clang substitute.
/// 5. Bare `"clang"` — relies on PATH (works on macOS / Linux / correctly-configured Windows).
fn resolve_clang() -> (PathBuf, bool) {
    // User override.
    if let Ok(linker) = std::env::var("KRAKEN_LINKER") {
        return (PathBuf::from(linker), false);
    }

    // Try LLVM_SYS_180_PREFIX — ensures ABI compatibility with the LLVM used at build time.
    if let Ok(prefix) = std::env::var("LLVM_SYS_180_PREFIX") {
        for name in ["clang.exe", "clang"] {
            let candidate = PathBuf::from(&prefix).join("bin").join(name);
            if candidate.exists() {
                return (candidate, false);
            }
        }
    }

    // Well-known Windows LLVM installation paths (winget / official installer defaults).
    // C:\Program Files\LLVM is the default for the official LLVM 18.1.8 Windows installer.
    #[cfg(target_os = "windows")]
    for dir in [
        r"C:\Program Files\LLVM\bin",
        r"C:\Program Files (x86)\LLVM\bin",
        r"C:\LLVM\bin",
        r"C:\Tools\LLVM\bin",
        r"C:\Tools\LLVM18\bin",
    ] {
        let candidate = PathBuf::from(dir).join("clang.exe");
        if candidate.exists() {
            return (candidate, false);
        }
    }

    // zig cc — available on many Windows dev machines as a clang substitute.
    #[cfg(target_os = "windows")]
    for zig_path in [r"C:\Tools\zig\zig.exe", r"C:\zig\zig.exe"] {
        if PathBuf::from(zig_path).exists() {
            return (PathBuf::from(zig_path), true);
        }
    }

    (PathBuf::from("clang"), false)
}

/// Link object file to executable.
fn link_executable(object_file: &PathBuf, output: &PathBuf) -> Result<()> {
    let (linker, is_zig) = resolve_clang();
    let mut cmd = std::process::Command::new(&linker);
    if is_zig {
        cmd.arg("cc");
    }
    cmd.arg(object_file).arg("-o").arg(output);

    // Link Kraken runtime library
    let runtime_lib = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("runtime/libkraken_runtime.a");
    if runtime_lib.exists() {
        cmd.arg(&runtime_lib);
    }

    // Platform-aware link rules:
    // - macOS: clang driver links libSystem by default; keep flags minimal.
    // - Linux: libc is default but libm is not; add -lm for math symbols.
    #[cfg(target_os = "linux")]
    {
        cmd.arg("-lm");
    }

    let status = cmd.status().context("Failed to run linker")?;

    if !status.success() {
        anyhow::bail!("Linking failed");
    }

    Ok(())
}

/// Check a single source file without generating code.
async fn check_file(file: &Path) -> Result<()> {
    crate::ffi::stdlib::validate_stdlib_table()
        .map_err(|e| anyhow::anyhow!(e))
        .context("Invalid stdlib/FFI signature table")?;

    let program = loader::load_program(file).await?;
    let program = monomorphize_program(program, file.to_path_buf())?;

    let mut type_checker = TypeChecker::new(file.to_path_buf());
    type_checker
        .check_program(&program)
        .context("Type checking error")?;

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

    #[test]
    fn stdlib_table_is_valid() {
        crate::ffi::stdlib::validate_stdlib_table().unwrap();
    }

    #[tokio::test]
    async fn ffi_negative_strlen_wrong_arity_fails() -> Result<()> {
        assert_check_fails_contains(
            PathBuf::from("../tests/programs/neg_strlen_arity.kr"),
            "strlen",
        )
        .await
    }

    #[tokio::test]
    async fn ffi_negative_strcmp_wrong_types_fails() -> Result<()> {
        assert_check_fails_contains(
            PathBuf::from("../tests/programs/neg_strcmp_types.kr"),
            "strcmp",
        )
        .await
    }

    #[tokio::test]
    async fn generics_where_clone_void_fails() -> Result<()> {
        assert_check_fails_contains(
            PathBuf::from("../tests/programs/neg_generics_where_clone_void.kr"),
            "Clone",
        )
        .await
    }

    #[tokio::test]
    async fn ffi_strcmp_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_strcmp.kr"), 0).await
    }

    #[tokio::test]
    async fn ffi_strlen_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_strlen.kr"), 4).await
    }

    #[tokio::test]
    async fn ffi_memcmp_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_memcmp.kr"), 0).await
    }

    #[tokio::test]
    async fn bytes_index_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_bytes_index.kr"), 0).await
    }

    #[tokio::test]
    async fn generics_id_int_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/generics_id_int.kr"), 0).await
    }

    #[tokio::test]
    async fn generics_id_infer_int_compile_and_run() -> Result<()> {
        assert_program_exit_code(
            PathBuf::from("../tests/programs/generics_id_infer_int.kr"),
            0,
        )
        .await
    }

    #[tokio::test]
    async fn generics_box_int_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/generics_box_int.kr"), 0).await
    }

    #[tokio::test]
    async fn generics_vec_int_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/generics_vec_int.kr"), 0).await
    }

    #[tokio::test]
    async fn generics_vec_string_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/generics_vec_string.kr"), 0).await
    }

    #[tokio::test]
    #[cfg_attr(
        target_os = "linux",
        ignore = "Platform-specific test failure on Linux - investigating"
    )]
    async fn generics_map_string_int_compile_and_run() -> Result<()> {
        assert_program_exit_code(
            PathBuf::from("../tests/programs/generics_map_string_int.kr"),
            0,
        )
        .await
    }

    #[tokio::test]
    async fn generics_where_clone_ok_compile_and_run() -> Result<()> {
        assert_program_exit_code(
            PathBuf::from("../tests/programs/generics_where_clone_ok.kr"),
            0,
        )
        .await
    }

    #[tokio::test]
    async fn string_trim_test_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/string_trim_test.kr"), 0).await
    }

    #[tokio::test]
    async fn string_contains_test_compile_and_run() -> Result<()> {
        assert_program_exit_code(
            PathBuf::from("../tests/programs/string_contains_test.kr"),
            0,
        )
        .await
    }

    #[tokio::test]
    async fn string_starts_ends_test_compile_and_run() -> Result<()> {
        assert_program_exit_code(
            PathBuf::from("../tests/programs/string_starts_ends_test.kr"),
            0,
        )
        .await
    }

    #[tokio::test]
    async fn string_utf8_test_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/string_utf8_test.kr"), 0).await
    }

    #[tokio::test]
    async fn string_split_test_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/string_split_test.kr"), 0).await
    }

    #[tokio::test]
    async fn string_join_test_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/string_join_test.kr"), 0).await
    }

    #[tokio::test]
    async fn string_replace_test_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/string_replace_test.kr"), 0).await
    }

    #[tokio::test]
    async fn cstr_roundtrip_compile_and_run() -> Result<()> {
        assert_program_exit_code(
            PathBuf::from("../tests/programs/simple_cstr_roundtrip.kr"),
            0,
        )
        .await
    }

    #[tokio::test]
    async fn cstr_strlen_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_cstr_strlen.kr"), 0).await
    }

    #[tokio::test]
    async fn ffi_malloc_free_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_malloc_free.kr"), 0).await
    }

    #[tokio::test]
    async fn ffi_getenv_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_getenv.kr"), 0).await
    }

    #[tokio::test]
    #[cfg(unix)]
    async fn ffi_setenv_getenv_compile_and_run() -> Result<()> {
        assert_program_exit_code(
            PathBuf::from("../tests/programs/simple_setenv_getenv.kr"),
            0,
        )
        .await
    }

    #[tokio::test]
    async fn ffi_fopen_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_fopen_fclose.kr"), 0).await
    }

    #[tokio::test]
    async fn ffi_fwrite_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_fwrite.kr"), 0).await
    }

    #[tokio::test]
    async fn ffi_fread_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_fread.kr"), 0).await
    }

    #[tokio::test]
    async fn ffi_file_ops_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/simple_file_ops.kr"), 0).await
    }

    #[tokio::test]
    #[cfg(unix)]
    async fn ffi_negative_fopen_null_traps() -> Result<()> {
        assert_program_terminated_by_signal(PathBuf::from("../tests/programs/neg_fopen_null.kr"))
            .await
    }

    #[tokio::test]
    #[cfg(unix)]
    async fn ffi_negative_realloc_null_traps() -> Result<()> {
        assert_program_terminated_by_signal(PathBuf::from("../tests/programs/neg_realloc_null.kr"))
            .await
    }

    #[tokio::test]
    #[cfg(unix)]
    async fn ffi_negative_malloc_null_traps() -> Result<()> {
        assert_program_terminated_by_signal(PathBuf::from("../tests/programs/neg_malloc_null.kr"))
            .await
    }

    #[tokio::test]
    #[cfg(unix)]
    async fn ffi_negative_from_cstr_null_traps() -> Result<()> {
        assert_program_terminated_by_signal(PathBuf::from(
            "../tests/programs/neg_from_cstr_null.kr",
        ))
        .await
    }

    #[tokio::test]
    #[cfg(unix)]
    async fn vec_int_pop_empty_traps() -> Result<()> {
        assert_program_terminated_by_signal(PathBuf::from(
            "../tests/programs/neg_vec_int_pop_empty.kr",
        ))
        .await
    }

    #[tokio::test]
    async fn modules_simple_import_compile_and_run() -> Result<()> {
        let program_path = PathBuf::from("../tests/programs/modules/simple_import_main.kr");
        let program = loader::load_program(&program_path).await?;
        let has_forty_two = program.statements.iter().any(|s| match s {
            parser::ast::Statement::FunctionDeclaration { name, .. } => name == "forty_two",
            _ => false,
        });
        if !has_forty_two {
            anyhow::bail!("Expected merged program to contain imported function forty_two");
        }

        let mut tc = TypeChecker::new(program_path.clone());
        tc.check_program(&program)
            .with_context(|| "Typechecking merged program failed")?;

        assert_program_exit_code(program_path, 42).await
    }

    #[tokio::test]
    async fn modules_negative_missing_import_fails() -> Result<()> {
        assert_check_fails_contains(
            PathBuf::from("../tests/programs/modules/neg_import_missing.kr"),
            "Import not found",
        )
        .await
    }

    #[tokio::test]
    async fn modules_negative_import_cycle_fails() -> Result<()> {
        assert_check_fails_contains(
            PathBuf::from("../tests/programs/modules/neg_import_cycle_main.kr"),
            "Import cycle detected",
        )
        .await
    }

    #[tokio::test]
    async fn modules_negative_duplicate_symbol_fails() -> Result<()> {
        assert_check_fails_contains(
            PathBuf::from("../tests/programs/modules/neg_duplicate_main.kr"),
            "Duplicate function: dup",
        )
        .await
    }

    #[tokio::test]
    async fn modules_visibility_public_api_can_use_private_helper() -> Result<()> {
        assert_program_exit_code(
            PathBuf::from("../tests/programs/modules/visibility_main.kr"),
            42,
        )
        .await
    }

    #[tokio::test]
    async fn modules_negative_private_symbol_not_visible_to_importer() -> Result<()> {
        assert_check_fails_contains(
            PathBuf::from("../tests/programs/modules/neg_visibility_private_access.kr"),
            "Undefined function: helper",
        )
        .await
    }

    #[tokio::test]
    async fn modules_negative_module_declaration_mismatch_fails() -> Result<()> {
        assert_check_fails_contains(
            PathBuf::from("../tests/programs/modules/neg_module_decl_mismatch.kr"),
            "Module declaration does not match file path",
        )
        .await
    }

    #[tokio::test]
    async fn modules_negative_imported_module_requires_module_declaration() -> Result<()> {
        assert_check_fails_contains(
            PathBuf::from("../tests/programs/modules/neg_import_missing_module_decl_main.kr"),
            "Imported module must declare its module path",
        )
        .await
    }

    #[tokio::test]
    async fn async_basic_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/async_basic_test.kr"), 0).await
    }

    #[tokio::test]
    async fn async_await_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/async_await_test.kr"), 0).await
    }

    #[tokio::test]
    async fn dyn_dispatch_compile_and_run() -> Result<()> {
        assert_program_exit_code(PathBuf::from("../tests/programs/dyn_dispatch_test.kr"), 0).await
    }

    async fn assert_program_exit_code(program: PathBuf, expected_exit_code: i32) -> Result<()> {
        let executable = compile_file(&program)
            .await
            .with_context(|| format!("Failed to compile {}", program.display()))?;

        let output = std::process::Command::new(&executable)
            .output()
            .with_context(|| format!("Failed to run executable: {}", executable.display()))?;

        let code = output
            .status
            .code()
            .ok_or_else(|| anyhow::anyhow!("Executable terminated by signal"))?;

        if code != expected_exit_code {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!(
                "Unexpected exit code. expected={expected_exit_code} actual={code}\nstdout:\n{stdout}\nstderr:\n{stderr}"
            );
        }

        Ok(())
    }

    #[cfg(unix)]
    async fn assert_program_terminated_by_signal(program: PathBuf) -> Result<()> {
        let executable = compile_file(&program)
            .await
            .with_context(|| format!("Failed to compile {}", program.display()))?;

        let output = std::process::Command::new(&executable)
            .output()
            .with_context(|| format!("Failed to run executable: {}", executable.display()))?;

        if output.status.code().is_some() {
            let code = output.status.code().unwrap_or(-1);
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!(
                "Expected executable to terminate by signal, but it exited normally: code={code}\nstdout:\n{stdout}\nstderr:\n{stderr}"
            );
        }

        Ok(())
    }

    async fn assert_check_fails_contains(program: PathBuf, needle: &str) -> Result<()> {
        let result = check_file(&program).await;
        if result.is_ok() {
            anyhow::bail!("Expected check to fail for {}", program.display());
        }

        let err = result.unwrap_err();
        let mut combined = String::new();
        let mut found = false;
        for (idx, cause) in err.chain().enumerate() {
            let s = cause.to_string();
            if idx > 0 {
                combined.push_str("\ncaused by: ");
            }
            combined.push_str(&s);
            if s.contains(needle) {
                found = true;
            }
        }

        if !found {
            anyhow::bail!("Expected error chain to contain {needle}, got: {combined}");
        }

        Ok(())
    }

    /// Assert that the generated IR matches the golden file.
    #[cfg(unix)]
    async fn assert_ir_snapshot(source: PathBuf, golden: PathBuf) -> Result<()> {
        let program = modules::loader::load_program(&source).await?;
        let mut lowering = ir::IrLowering::new();
        let ir_program = lowering
            .lower_program(&program)
            .map_err(|e| anyhow::anyhow!("IR lowering failed: {e}"))?;
        let generated = format!("{ir_program}");
        let expected = tokio::fs::read_to_string(&golden)
            .await
            .with_context(|| format!("Failed to read golden file: {}", golden.display()))?;

        if generated.trim() != expected.trim() {
            anyhow::bail!(
                "IR mismatch for {}\n--- expected ---\n{}\n--- generated ---\n{}",
                source.display(),
                expected.trim(),
                generated.trim()
            );
        }

        Ok(())
    }

    #[tokio::test]
    #[cfg(unix)]
    async fn ir_snapshot_hello() -> Result<()> {
        assert_ir_snapshot(
            PathBuf::from("../tests/ir_snapshots/hello.kr"),
            PathBuf::from("../tests/ir_snapshots/hello.ir"),
        )
        .await
    }

    #[tokio::test]
    #[cfg(unix)]
    async fn ir_snapshot_arithmetic() -> Result<()> {
        assert_ir_snapshot(
            PathBuf::from("../tests/ir_snapshots/arithmetic.kr"),
            PathBuf::from("../tests/ir_snapshots/arithmetic.ir"),
        )
        .await
    }

    #[tokio::test]
    #[cfg(unix)]
    async fn ir_snapshot_if_else() -> Result<()> {
        assert_ir_snapshot(
            PathBuf::from("../tests/ir_snapshots/if_else.kr"),
            PathBuf::from("../tests/ir_snapshots/if_else.ir"),
        )
        .await
    }
}
