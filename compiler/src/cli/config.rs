//! CLI configuration and settings.

use std::path::PathBuf;

/// CLI configuration
#[derive(Debug, Clone)]
pub struct CliConfig {
    /// Enable colored output
    pub color: bool,
    /// Quiet mode (minimal output)
    pub quiet: bool,
    /// Verbose mode (detailed output)
    pub verbose: bool,
    /// Output format (text, json)
    pub format: OutputFormat,
    /// Number of parallel jobs
    pub jobs: usize,
    /// Project root directory
    pub project_root: Option<PathBuf>,
}

impl Default for CliConfig {
    fn default() -> Self {
        Self {
            color: true,
            quiet: false,
            verbose: false,
            format: OutputFormat::Text,
            jobs: num_cpus::get(),
            project_root: None,
        }
    }
}

/// Output format options
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Human-readable text output
    Text,
    /// JSON output for tooling
    Json,
    /// Quiet mode (errors only)
    Quiet,
}

impl CliConfig {
    /// Create a new CLI configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Set color output
    pub fn with_color(mut self, color: bool) -> Self {
        self.color = color;
        self
    }

    /// Set quiet mode
    pub fn with_quiet(mut self, quiet: bool) -> Self {
        self.quiet = quiet;
        self
    }

    /// Set verbose mode
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Set output format
    pub fn with_format(mut self, format: OutputFormat) -> Self {
        self.format = format;
        self
    }

    /// Set number of parallel jobs
    pub fn with_jobs(mut self, jobs: usize) -> Self {
        self.jobs = jobs.max(1);
        self
    }

    /// Set project root directory
    pub fn with_project_root(mut self, root: PathBuf) -> Self {
        self.project_root = Some(root);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = CliConfig::default();
        assert!(config.color);
        assert!(!config.quiet);
        assert!(!config.verbose);
        assert_eq!(config.format, OutputFormat::Text);
        assert!(config.jobs > 0);
    }

    #[test]
    fn test_config_builder() {
        let config = CliConfig::new()
            .with_color(false)
            .with_quiet(true)
            .with_jobs(4);

        assert!(!config.color);
        assert!(config.quiet);
        assert_eq!(config.jobs, 4);
    }

    #[test]
    fn test_jobs_minimum() {
        let config = CliConfig::new().with_jobs(0);
        assert_eq!(config.jobs, 1);
    }

    #[test]
    fn test_with_verbose() {
        let config = CliConfig::new().with_verbose(true);
        assert!(config.verbose);
    }

    #[test]
    fn test_with_format() {
        let config = CliConfig::new().with_format(OutputFormat::Json);
        assert_eq!(config.format, OutputFormat::Json);
        let config2 = CliConfig::new().with_format(OutputFormat::Quiet);
        assert_eq!(config2.format, OutputFormat::Quiet);
    }

    #[test]
    fn test_with_project_root() {
        let config = CliConfig::new().with_project_root(PathBuf::from("/tmp"));
        assert_eq!(config.project_root, Some(PathBuf::from("/tmp")));
    }

    #[test]
    fn test_new_equals_default() {
        let a = CliConfig::new();
        let b = CliConfig::default();
        assert_eq!(a.color, b.color);
        assert_eq!(a.quiet, b.quiet);
        assert_eq!(a.verbose, b.verbose);
        assert_eq!(a.format, b.format);
    }

    #[test]
    fn test_full_builder_chain() {
        let config = CliConfig::new()
            .with_color(false)
            .with_quiet(true)
            .with_verbose(true)
            .with_format(OutputFormat::Json)
            .with_jobs(8)
            .with_project_root(PathBuf::from("/project"));
        assert!(!config.color);
        assert!(config.quiet);
        assert!(config.verbose);
        assert_eq!(config.format, OutputFormat::Json);
        assert_eq!(config.jobs, 8);
        assert_eq!(config.project_root, Some(PathBuf::from("/project")));
    }
}
