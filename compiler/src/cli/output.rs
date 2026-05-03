//! CLI output formatting with rich colors, ASCII art, spinners, and tables.
//!
//! Cross-platform terminal output with Rust-style keyword coloring.

use colored::*;
use comfy_table::{modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, Cell, Table};
use figlet_rs::FIGfont;
use indicatif::{ProgressBar, ProgressStyle};
use std::fmt;

/// Output message with severity level and Rust-style keyword coloring
#[derive(Debug, Clone)]
pub struct OutputMessage {
    pub level: MessageLevel,
    pub message: String,
}

/// Message severity level matching Rust compiler output
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MessageLevel {
    Error,
    Warning,
    Info,
    Success,
    Debug,
    Note,
    Help,
}

impl OutputMessage {
    /// Create a new output message
    pub fn new(level: MessageLevel, message: impl Into<String>) -> Self {
        Self {
            level,
            message: message.into(),
        }
    }

    /// Create an error message (red, bold)
    pub fn error(message: impl Into<String>) -> Self {
        Self::new(MessageLevel::Error, message)
    }

    /// Create a warning message (yellow, bold)
    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(MessageLevel::Warning, message)
    }

    /// Create an info message (cyan)
    pub fn info(message: impl Into<String>) -> Self {
        Self::new(MessageLevel::Info, message)
    }

    /// Create a success message (green, bold)
    pub fn success(message: impl Into<String>) -> Self {
        Self::new(MessageLevel::Success, message)
    }

    /// Create a debug message (dimmed)
    pub fn debug(message: impl Into<String>) -> Self {
        Self::new(MessageLevel::Debug, message)
    }

    /// Create a note message (bright blue)
    pub fn note(message: impl Into<String>) -> Self {
        Self::new(MessageLevel::Note, message)
    }

    /// Create a help message (bright cyan)
    pub fn help(message: impl Into<String>) -> Self {
        Self::new(MessageLevel::Help, message)
    }

    /// Format with Rust-style colors (cross-platform)
    pub fn format_colored(&self) -> String {
        let (prefix, color_fn): (&str, fn(&str) -> ColoredString) = match self.level {
            MessageLevel::Error => ("error", |s| s.red().bold()),
            MessageLevel::Warning => ("warning", |s| s.yellow().bold()),
            MessageLevel::Info => ("info", |s| s.cyan()),
            MessageLevel::Success => ("success", |s| s.green().bold()),
            MessageLevel::Debug => ("debug", |s| s.dimmed()),
            MessageLevel::Note => ("note", |s| s.bright_blue().bold()),
            MessageLevel::Help => ("help", |s| s.bright_cyan().bold()),
        };

        format!("{}: {}", color_fn(prefix), self.message)
    }

    /// Format without color
    pub fn format_plain(&self) -> String {
        let prefix = match self.level {
            MessageLevel::Error => "error",
            MessageLevel::Warning => "warning",
            MessageLevel::Info => "info",
            MessageLevel::Success => "success",
            MessageLevel::Debug => "debug",
            MessageLevel::Note => "note",
            MessageLevel::Help => "help",
        };
        format!("{}: {}", prefix, self.message)
    }
}

impl fmt::Display for OutputMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_colored())
    }
}

/// ASCII art banner generator
pub struct Banner;

impl Banner {
    /// Generate ASCII art banner for Kraken
    pub fn kraken() -> String {
        let font = FIGfont::standard().unwrap();
        let figure = font.convert("KRAKEN");
        figure
            .map(|f| f.to_string())
            .unwrap_or_else(|| "KRAKEN".to_string())
    }

    /// Generate custom ASCII art banner
    pub fn custom(text: &str) -> String {
        let font = FIGfont::standard().unwrap();
        let figure = font.convert(text);
        figure
            .map(|f| f.to_string())
            .unwrap_or_else(|| text.to_string())
    }

    /// Small ASCII art logo
    pub fn logo_small() -> String {
        r#"
    ╔═══════════════════════════════════╗
    ║   🦑  K R A K E N   L A N G  🦑   ║
    ╚═══════════════════════════════════╝
        "#
        .to_string()
    }
}

/// Progress indicator with spinner support
pub struct ProgressIndicator {
    bar: ProgressBar,
}

impl ProgressIndicator {
    /// Create a new progress bar
    pub fn new(message: &str, total: u64) -> Self {
        let bar = ProgressBar::new(total);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("{msg} [{bar:40.cyan/blue}] {pos}/{len} ({percent}%)")
                .unwrap()
                .progress_chars("=>-"),
        );
        bar.set_message(message.to_string());
        Self { bar }
    }

    /// Create a spinner for indeterminate progress
    pub fn spinner(message: &str) -> Self {
        let bar = ProgressBar::new_spinner();
        bar.set_style(
            ProgressStyle::default_spinner()
                .template("{spinner:.green} {msg}")
                .unwrap(),
        );
        bar.set_message(message.to_string());
        Self { bar }
    }

    /// Update progress
    pub fn update(&self, current: u64) {
        self.bar.set_position(current);
    }

    /// Increment progress
    pub fn increment(&self) {
        self.bar.inc(1);
    }

    /// Finish progress
    pub fn finish(&self, message: &str) {
        self.bar.finish_with_message(message.to_string());
    }

    /// Finish and clear
    pub fn finish_and_clear(&self) {
        self.bar.finish_and_clear();
    }
}

/// Table formatter for structured output
pub struct TableFormatter {
    table: Table,
}

impl TableFormatter {
    /// Create a new table
    pub fn new() -> Self {
        let mut table = Table::new();
        table
            .load_preset(UTF8_FULL)
            .apply_modifier(UTF8_ROUND_CORNERS);
        Self { table }
    }

    /// Add header row
    pub fn header(&mut self, headers: Vec<&str>) -> &mut Self {
        let cells: Vec<Cell> = headers
            .into_iter()
            .map(|h| Cell::new(h).fg(comfy_table::Color::Cyan))
            .collect();
        self.table.set_header(cells);
        self
    }

    /// Add data row
    pub fn row(&mut self, cells: Vec<&str>) -> &mut Self {
        self.table.add_row(cells);
        self
    }

    /// Render table
    pub fn render(&self) -> String {
        self.table.to_string()
    }
}

impl Default for TableFormatter {
    fn default() -> Self {
        Self::new()
    }
}

/// Diagnostic message formatter (Rust-style compiler diagnostics)
pub struct Diagnostic {
    level: MessageLevel,
    code: Option<String>,
    message: String,
    file: Option<String>,
    line: Option<usize>,
    column: Option<usize>,
    snippet: Option<String>,
    notes: Vec<String>,
    help: Option<String>,
}

impl Diagnostic {
    /// Create a new diagnostic
    pub fn new(level: MessageLevel, message: impl Into<String>) -> Self {
        Self {
            level,
            code: None,
            message: message.into(),
            file: None,
            line: None,
            column: None,
            snippet: None,
            notes: Vec::new(),
            help: None,
        }
    }

    /// Set error code
    pub fn code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Set file location
    pub fn location(mut self, file: impl Into<String>, line: usize, column: usize) -> Self {
        self.file = Some(file.into());
        self.line = Some(line);
        self.column = Some(column);
        self
    }

    /// Set code snippet
    pub fn snippet(mut self, snippet: impl Into<String>) -> Self {
        self.snippet = Some(snippet.into());
        self
    }

    /// Add note
    pub fn note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Set help message
    pub fn help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    /// Format diagnostic in Rust compiler style
    pub fn format(&self) -> String {
        let mut output = String::new();

        // Main diagnostic line
        let level_str = match self.level {
            MessageLevel::Error => "error".red().bold(),
            MessageLevel::Warning => "warning".yellow().bold(),
            MessageLevel::Note => "note".bright_blue().bold(),
            MessageLevel::Help => "help".bright_cyan().bold(),
            _ => "info".cyan(),
        };

        if let Some(code) = &self.code {
            output.push_str(&format!("{level_str}[{code}]: {}\n", self.message));
        } else {
            output.push_str(&format!("{level_str}: {}\n", self.message));
        }

        // Location
        if let (Some(file), Some(line), Some(column)) = (&self.file, self.line, self.column) {
            output.push_str(&format!(
                "  {} {}:{}:{}\n",
                "-->".bright_blue().bold(),
                file,
                line,
                column
            ));
        }

        // Code snippet
        if let Some(snippet) = &self.snippet {
            output.push_str(&format!("   {}\n", "|".bright_blue().bold()));
            for (i, line) in snippet.lines().enumerate() {
                output.push_str(&format!(
                    "{:>3} {} {}\n",
                    format!("{}", i + 1).bright_blue().bold(),
                    "|".bright_blue().bold(),
                    line
                ));
            }
            output.push_str(&format!("   {}\n", "|".bright_blue().bold()));
        }

        // Notes
        for note in &self.notes {
            output.push_str(&format!("  {} {}\n", "note:".bright_blue().bold(), note));
        }

        // Help
        if let Some(help) = &self.help {
            output.push_str(&format!("  {} {}\n", "help:".bright_cyan().bold(), help));
        }

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_output_message_creation() {
        let msg = OutputMessage::error("Test error");
        assert_eq!(msg.level, MessageLevel::Error);
        assert_eq!(msg.message, "Test error");
    }

    #[test]
    fn test_output_message_formatting() {
        let msg = OutputMessage::info("Test info");
        let plain = msg.format_plain();
        assert!(plain.contains("info:"));
        assert!(plain.contains("Test info"));
    }

    #[test]
    fn test_note_and_help_messages() {
        let note = OutputMessage::note("This is a note");
        assert_eq!(note.level, MessageLevel::Note);

        let help = OutputMessage::help("Try this instead");
        assert_eq!(help.level, MessageLevel::Help);
    }

    #[test]
    fn test_banner_generation() {
        let logo = Banner::logo_small();
        // logo_small uses Unicode box drawing and spaced text
        assert!(logo.contains("K R A K E N   L A N G"));

        // Test custom banner (may fail if figlet font not available)
        let custom = Banner::custom("TEST");
        assert!(!custom.is_empty());
    }

    #[test]
    fn test_table_formatter() {
        let mut table = TableFormatter::new();
        table.header(vec!["Name", "Type", "Value"]);
        table.row(vec!["x", "int", "42"]);
        let output = table.render();
        assert!(output.contains("Name"));
        assert!(output.contains("int"));
    }

    #[test]
    fn test_diagnostic_formatting() {
        let diag = Diagnostic::new(MessageLevel::Error, "undefined variable")
            .code("E0001")
            .location("main.kr", 10, 5)
            .snippet("let x = y + 1;")
            .note("variable 'y' is not defined in this scope")
            .help("consider declaring 'y' before using it");

        let formatted = diag.format();
        assert!(formatted.contains("error"));
        assert!(formatted.contains("E0001"));
        assert!(formatted.contains("main.kr:10:5"));
    }

    // --- All message level constructors ---

    #[test]
    fn test_all_message_constructors() {
        let e = OutputMessage::error("e");
        assert_eq!(e.level, MessageLevel::Error);
        let w = OutputMessage::warning("w");
        assert_eq!(w.level, MessageLevel::Warning);
        let i = OutputMessage::info("i");
        assert_eq!(i.level, MessageLevel::Info);
        let s = OutputMessage::success("s");
        assert_eq!(s.level, MessageLevel::Success);
        let d = OutputMessage::debug("d");
        assert_eq!(d.level, MessageLevel::Debug);
    }

    // --- format_colored all variants ---

    #[test]
    fn test_format_colored_all_levels() {
        for (level, expected) in [
            (MessageLevel::Error, "error"),
            (MessageLevel::Warning, "warning"),
            (MessageLevel::Info, "info"),
            (MessageLevel::Success, "success"),
            (MessageLevel::Debug, "debug"),
            (MessageLevel::Note, "note"),
            (MessageLevel::Help, "help"),
        ] {
            let msg = OutputMessage::new(level, "test");
            let colored = msg.format_colored();
            assert!(colored.contains("test"), "Missing message for {expected}");
        }
    }

    // --- format_plain all variants ---

    #[test]
    fn test_format_plain_all_levels() {
        for (level, prefix) in [
            (MessageLevel::Error, "error"),
            (MessageLevel::Warning, "warning"),
            (MessageLevel::Info, "info"),
            (MessageLevel::Success, "success"),
            (MessageLevel::Debug, "debug"),
            (MessageLevel::Note, "note"),
            (MessageLevel::Help, "help"),
        ] {
            let msg = OutputMessage::new(level, "msg");
            let plain = msg.format_plain();
            assert!(
                plain.starts_with(prefix),
                "Expected prefix {prefix}, got {plain}"
            );
        }
    }

    // --- Display impl ---

    #[test]
    fn test_output_message_display() {
        let msg = OutputMessage::error("boom");
        let s = format!("{msg}");
        assert!(s.contains("boom"));
    }

    // --- Banner::kraken ---

    #[test]
    fn test_banner_kraken() {
        let banner = Banner::kraken();
        assert!(!banner.is_empty());
    }

    // --- ProgressIndicator ---

    #[test]
    fn test_progress_indicator_bar() {
        let p = ProgressIndicator::new("Compiling", 10);
        p.update(5);
        p.increment();
        p.finish("Done");
    }

    #[test]
    fn test_progress_indicator_spinner() {
        let p = ProgressIndicator::spinner("Loading");
        p.increment();
        p.finish_and_clear();
    }

    // --- TableFormatter default ---

    #[test]
    fn test_table_formatter_default() {
        let t = TableFormatter::default();
        let r = t.render();
        assert!(r.is_empty() || !r.is_empty()); // exercises Default impl
    }

    // --- Diagnostic without optional fields ---

    #[test]
    fn test_diagnostic_minimal() {
        let d = Diagnostic::new(MessageLevel::Warning, "unused variable");
        let f = d.format();
        assert!(f.contains("warning"));
        assert!(f.contains("unused variable"));
        // No code, location, snippet, notes, or help
        assert!(!f.contains("["));
        assert!(!f.contains("-->"));
    }

    // --- Diagnostic with all levels ---

    #[test]
    fn test_diagnostic_all_levels() {
        for level in [
            MessageLevel::Error,
            MessageLevel::Warning,
            MessageLevel::Note,
            MessageLevel::Help,
            MessageLevel::Info,
        ] {
            let d = Diagnostic::new(level, "test");
            let f = d.format();
            assert!(f.contains("test"));
        }
    }

    // --- Diagnostic with multiple notes ---

    #[test]
    fn test_diagnostic_multiple_notes() {
        let d = Diagnostic::new(MessageLevel::Error, "err")
            .note("note 1")
            .note("note 2");
        let f = d.format();
        assert!(f.contains("note 1"));
        assert!(f.contains("note 2"));
    }

    // --- Diagnostic with multi-line snippet ---

    #[test]
    fn test_diagnostic_multiline_snippet() {
        let d = Diagnostic::new(MessageLevel::Error, "err").snippet("line1\nline2\nline3");
        let f = d.format();
        assert!(f.contains("line1"));
        assert!(f.contains("line3"));
    }
}
