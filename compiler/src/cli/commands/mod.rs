//! Command implementations for the Kraken CLI.

/// Benchmark runner with statistical analysis and baseline comparison.
pub mod bench;
/// Build command: compile a Kraken project with configurable optimization levels.
pub mod build;
/// Check command: type-check a project without emitting binaries.
pub mod check;
/// Clean command: remove build artifacts from the output directory.
pub mod clean;
/// Doc command: generate HTML documentation from doc comments.
pub mod doc;
/// Fmt command: format Kraken source files with whitespace normalization.
pub mod fmt;
/// Init command: initialize a new Kraken project in the current directory.
pub mod init;
/// New command: create a new Kraken project from a template.
pub mod new;
/// Run command: build and execute a Kraken project.
pub mod run;
/// Test command: discover and run tests from the `tests/` directory.
pub mod test;
/// Version command: display compiler version information.
pub mod version;
