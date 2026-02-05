//! Logging facade for Kraken - equivalent to Rust's log crate.

#![allow(dead_code)]

use std::fmt;
use std::sync::{Arc, Mutex};

/// Log level enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Trace = 0,
    Debug = 1,
    Info = 2,
    Warn = 3,
    Error = 4,
}

impl LogLevel {
    /// Get the string representation of the log level
    pub fn as_str(&self) -> &'static str {
        match self {
            LogLevel::Trace => "TRACE",
            LogLevel::Debug => "DEBUG",
            LogLevel::Info => "INFO",
            LogLevel::Warn => "WARN",
            LogLevel::Error => "ERROR",
        }
    }

    /// Parse a log level from a string
    pub fn parse_level(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "TRACE" => Some(LogLevel::Trace),
            "DEBUG" => Some(LogLevel::Debug),
            "INFO" => Some(LogLevel::Info),
            "WARN" | "WARNING" => Some(LogLevel::Warn),
            "ERROR" => Some(LogLevel::Error),
            _ => None,
        }
    }
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Log record containing all information about a log event
#[derive(Debug, Clone)]
pub struct LogRecord {
    pub level: LogLevel,
    pub target: String,
    pub message: String,
    pub file: Option<String>,
    pub line: Option<u32>,
    pub module_path: Option<String>,
}

impl LogRecord {
    /// Create a new log record
    pub fn new(level: LogLevel, target: String, message: String) -> Self {
        Self {
            level,
            target,
            message,
            file: None,
            line: None,
            module_path: None,
        }
    }

    /// Set the file location
    pub fn with_file(mut self, file: String) -> Self {
        self.file = Some(file);
        self
    }

    /// Set the line number
    pub fn with_line(mut self, line: u32) -> Self {
        self.line = Some(line);
        self
    }

    /// Set the module path
    pub fn with_module_path(mut self, module_path: String) -> Self {
        self.module_path = Some(module_path);
        self
    }
}

/// Trait for log implementations
pub trait Logger: Send + Sync {
    /// Log a record
    fn log(&self, record: &LogRecord);

    /// Check if a log level is enabled
    fn enabled(&self, level: LogLevel) -> bool;

    /// Flush any buffered logs
    fn flush(&self);
}

/// Simple console logger implementation
pub struct ConsoleLogger {
    level: LogLevel,
}

impl ConsoleLogger {
    /// Create a new console logger with the specified level
    pub fn new(level: LogLevel) -> Self {
        Self { level }
    }
}

impl Logger for ConsoleLogger {
    fn log(&self, record: &LogRecord) {
        if record.level >= self.level {
            let location = if let (Some(file), Some(line)) = (&record.file, record.line) {
                format!(" [{file}:{line}]")
            } else {
                String::new()
            };

            eprintln!(
                "[{}] {}{}: {}",
                record.level, record.target, location, record.message
            );
        }
    }

    fn enabled(&self, level: LogLevel) -> bool {
        level >= self.level
    }

    fn flush(&self) {
        // Console output is typically unbuffered
    }
}

/// Global logger state
static LOGGER: Mutex<Option<Arc<dyn Logger>>> = Mutex::new(None);
static MAX_LEVEL: Mutex<LogLevel> = Mutex::new(LogLevel::Info);

/// Set the global logger
pub fn set_logger(logger: Arc<dyn Logger>) {
    let mut global_logger = LOGGER.lock().unwrap();
    *global_logger = Some(logger);
}

/// Set the maximum log level
pub fn set_max_level(level: LogLevel) {
    let mut max_level = MAX_LEVEL.lock().unwrap();
    *max_level = level;
}

/// Get the maximum log level
pub fn max_level() -> LogLevel {
    *MAX_LEVEL.lock().unwrap()
}

/// Check if a log level is enabled
pub fn log_enabled(level: LogLevel) -> bool {
    level >= max_level()
}

/// Log a message at the specified level
pub fn log(level: LogLevel, target: &str, message: &str) {
    if !log_enabled(level) {
        return;
    }

    let record = LogRecord::new(level, target.to_string(), message.to_string());

    if let Some(logger) = LOGGER.lock().unwrap().as_ref() {
        logger.log(&record);
    }
}

/// Log a trace message
pub fn trace(target: &str, message: &str) {
    log(LogLevel::Trace, target, message);
}

/// Log a debug message
pub fn debug(target: &str, message: &str) {
    log(LogLevel::Debug, target, message);
}

/// Log an info message
pub fn info(target: &str, message: &str) {
    log(LogLevel::Info, target, message);
}

/// Log a warning message
pub fn warn(target: &str, message: &str) {
    log(LogLevel::Warn, target, message);
}

/// Log an error message
pub fn error(target: &str, message: &str) {
    log(LogLevel::Error, target, message);
}

/// Flush all loggers
pub fn flush() {
    if let Some(logger) = LOGGER.lock().unwrap().as_ref() {
        logger.flush();
    }
}

/// Logging utilities
pub struct Log;

impl Log {
    /// Initialize logging with a console logger
    pub fn init(level: LogLevel) {
        let logger = Arc::new(ConsoleLogger::new(level));
        set_logger(logger);
        set_max_level(level);
    }

    /// Initialize logging with a custom logger
    pub fn init_with_logger(logger: Arc<dyn Logger>, level: LogLevel) {
        set_logger(logger);
        set_max_level(level);
    }

    /// Log a trace message
    pub fn trace(target: &str, message: &str) {
        trace(target, message);
    }

    /// Log a debug message
    pub fn debug(target: &str, message: &str) {
        debug(target, message);
    }

    /// Log an info message
    pub fn info(target: &str, message: &str) {
        info(target, message);
    }

    /// Log a warning message
    pub fn warn(target: &str, message: &str) {
        warn(target, message);
    }

    /// Log an error message
    pub fn error(target: &str, message: &str) {
        error(target, message);
    }

    /// Check if a log level is enabled
    pub fn enabled(level: LogLevel) -> bool {
        log_enabled(level)
    }

    /// Flush all loggers
    pub fn flush() {
        flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_level_ordering() {
        assert!(LogLevel::Trace < LogLevel::Debug);
        assert!(LogLevel::Debug < LogLevel::Info);
        assert!(LogLevel::Info < LogLevel::Warn);
        assert!(LogLevel::Warn < LogLevel::Error);
    }

    #[test]
    fn test_log_level_as_str() {
        assert_eq!(LogLevel::Trace.as_str(), "TRACE");
        assert_eq!(LogLevel::Debug.as_str(), "DEBUG");
        assert_eq!(LogLevel::Info.as_str(), "INFO");
        assert_eq!(LogLevel::Warn.as_str(), "WARN");
        assert_eq!(LogLevel::Error.as_str(), "ERROR");
    }

    #[test]
    fn test_log_level_parse() {
        assert_eq!(LogLevel::parse_level("TRACE"), Some(LogLevel::Trace));
        assert_eq!(LogLevel::parse_level("DEBUG"), Some(LogLevel::Debug));
        assert_eq!(LogLevel::parse_level("INFO"), Some(LogLevel::Info));
        assert_eq!(LogLevel::parse_level("WARN"), Some(LogLevel::Warn));
        assert_eq!(LogLevel::parse_level("WARNING"), Some(LogLevel::Warn));
        assert_eq!(LogLevel::parse_level("ERROR"), Some(LogLevel::Error));
        assert_eq!(LogLevel::parse_level("INVALID"), None);
    }

    #[test]
    fn test_log_level_parse_case_insensitive() {
        assert_eq!(LogLevel::parse_level("trace"), Some(LogLevel::Trace));
        assert_eq!(LogLevel::parse_level("Debug"), Some(LogLevel::Debug));
        assert_eq!(LogLevel::parse_level("iNfO"), Some(LogLevel::Info));
    }

    #[test]
    fn test_log_record_creation() {
        let record = LogRecord::new(
            LogLevel::Info,
            "test_target".to_string(),
            "test message".to_string(),
        );

        assert_eq!(record.level, LogLevel::Info);
        assert_eq!(record.target, "test_target");
        assert_eq!(record.message, "test message");
        assert!(record.file.is_none());
        assert!(record.line.is_none());
    }

    #[test]
    fn test_log_record_with_location() {
        let record = LogRecord::new(
            LogLevel::Info,
            "test_target".to_string(),
            "test message".to_string(),
        )
        .with_file("test.rs".to_string())
        .with_line(42);

        assert_eq!(record.file, Some("test.rs".to_string()));
        assert_eq!(record.line, Some(42));
    }

    #[test]
    fn test_console_logger() {
        let logger = ConsoleLogger::new(LogLevel::Info);
        assert!(logger.enabled(LogLevel::Info));
        assert!(logger.enabled(LogLevel::Warn));
        assert!(logger.enabled(LogLevel::Error));
        assert!(!logger.enabled(LogLevel::Debug));
        assert!(!logger.enabled(LogLevel::Trace));
    }

    #[test]
    fn test_console_logger_log() {
        let logger = ConsoleLogger::new(LogLevel::Info);
        let record = LogRecord::new(
            LogLevel::Info,
            "test".to_string(),
            "test message".to_string(),
        );
        logger.log(&record);
    }

    #[test]
    fn test_log_init() {
        Log::init(LogLevel::Debug);
        assert!(Log::enabled(LogLevel::Debug));
        assert!(Log::enabled(LogLevel::Info));
        assert!(!Log::enabled(LogLevel::Trace));
    }

    #[test]
    fn test_log_functions() {
        Log::init(LogLevel::Trace);
        Log::trace("test", "trace message");
        Log::debug("test", "debug message");
        Log::info("test", "info message");
        Log::warn("test", "warn message");
        Log::error("test", "error message");
        Log::flush();
    }

    #[test]
    fn test_max_level() {
        set_max_level(LogLevel::Warn);
        assert_eq!(max_level(), LogLevel::Warn);
        assert!(log_enabled(LogLevel::Warn));
        assert!(log_enabled(LogLevel::Error));
        assert!(!log_enabled(LogLevel::Info));
    }

    #[test]
    fn test_log_level_display() {
        assert_eq!(format!("{}", LogLevel::Trace), "TRACE");
        assert_eq!(format!("{}", LogLevel::Debug), "DEBUG");
        assert_eq!(format!("{}", LogLevel::Info), "INFO");
        assert_eq!(format!("{}", LogLevel::Warn), "WARN");
        assert_eq!(format!("{}", LogLevel::Error), "ERROR");
    }
}
