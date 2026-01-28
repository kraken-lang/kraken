use std::collections::HashMap;
use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
    Fatal,
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogLevel::Trace => write!(f, "TRACE"),
            LogLevel::Debug => write!(f, "DEBUG"),
            LogLevel::Info => write!(f, "INFO"),
            LogLevel::Warn => write!(f, "WARN"),
            LogLevel::Error => write!(f, "ERROR"),
            LogLevel::Fatal => write!(f, "FATAL"),
        }
    }
}

pub struct Logger {
    level: LogLevel,
    formatter: Box<dyn Fn(&LogRecord) -> String>,
}

pub struct LogRecord {
    pub level: LogLevel,
    pub message: String,
    pub timestamp: u64,
}

impl Logger {
    pub fn new(level: LogLevel) -> Self {
        Logger {
            level,
            formatter: Box::new(Self::default_formatter),
        }
    }

    pub fn with_formatter<F>(mut self, formatter: F) -> Self
    where
        F: Fn(&LogRecord) -> String + 'static,
    {
        self.formatter = Box::new(formatter);
        self
    }

    pub fn set_level(&mut self, level: LogLevel) {
        self.level = level;
    }

    pub fn log(&self, level: LogLevel, message: &str) {
        if level >= self.level {
            let record = LogRecord {
                level,
                message: message.to_string(),
                timestamp: SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs(),
            };
            println!("{}", (self.formatter)(&record));
        }
    }

    pub fn trace(&self, message: &str) {
        self.log(LogLevel::Trace, message);
    }

    pub fn debug(&self, message: &str) {
        self.log(LogLevel::Debug, message);
    }

    pub fn info(&self, message: &str) {
        self.log(LogLevel::Info, message);
    }

    pub fn warn(&self, message: &str) {
        self.log(LogLevel::Warn, message);
    }

    pub fn error(&self, message: &str) {
        self.log(LogLevel::Error, message);
    }

    pub fn fatal(&self, message: &str) {
        self.log(LogLevel::Fatal, message);
    }

    fn default_formatter(record: &LogRecord) -> String {
        format!(
            "[{}] {} - {}",
            record.timestamp, record.level, record.message
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Uuid {
    bytes: [u8; 16],
}

impl Uuid {
    pub fn new_v4() -> Self {
        let mut bytes = [0u8; 16];
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();

        for (i, byte) in bytes.iter_mut().enumerate() {
            *byte = ((timestamp >> (i * 8)) & 0xff) as u8;
        }

        bytes[6] = (bytes[6] & 0x0f) | 0x40;
        bytes[8] = (bytes[8] & 0x3f) | 0x80;

        Uuid { bytes }
    }

    pub fn nil() -> Self {
        Uuid { bytes: [0; 16] }
    }

    pub fn from_bytes(bytes: [u8; 16]) -> Self {
        Uuid { bytes }
    }

    pub fn as_bytes(&self) -> &[u8; 16] {
        &self.bytes
    }

    pub fn as_hyphenated(&self) -> String {
        format!(
            "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
            self.bytes[0], self.bytes[1], self.bytes[2], self.bytes[3],
            self.bytes[4], self.bytes[5],
            self.bytes[6], self.bytes[7],
            self.bytes[8], self.bytes[9],
            self.bytes[10], self.bytes[11], self.bytes[12], self.bytes[13], self.bytes[14], self.bytes[15]
        )
    }

    pub fn parse(s: &str) -> Result<Self, String> {
        let s = s.replace('-', "");
        if s.len() != 32 {
            return Err("Invalid UUID length".to_string());
        }

        let mut bytes = [0u8; 16];
        for (i, byte) in bytes.iter_mut().enumerate() {
            let hex = &s[i * 2..i * 2 + 2];
            *byte = u8::from_str_radix(hex, 16).map_err(|_| "Invalid hex digit")?;
        }

        Ok(Uuid { bytes })
    }

    pub fn is_nil(&self) -> bool {
        self.bytes.iter().all(|&b| b == 0)
    }
}

impl fmt::Display for Uuid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_hyphenated())
    }
}

pub struct CliParser {
    args: Vec<String>,
    options: HashMap<String, Option<String>>,
    positional: Vec<String>,
}

impl CliParser {
    pub fn new(args: Vec<String>) -> Self {
        let mut parser = CliParser {
            args: args.clone(),
            options: HashMap::new(),
            positional: Vec::new(),
        };
        parser.parse();
        parser
    }

    pub fn from_env() -> Self {
        Self::new(std::env::args().collect())
    }

    fn parse(&mut self) {
        let mut i = 1;
        while i < self.args.len() {
            let arg = &self.args[i];
            if let Some(stripped) = arg.strip_prefix("--") {
                let key = stripped.to_string();
                if i + 1 < self.args.len() && !self.args[i + 1].starts_with('-') {
                    self.options.insert(key, Some(self.args[i + 1].clone()));
                    i += 2;
                } else {
                    self.options.insert(key, None);
                    i += 1;
                }
            } else if let Some(stripped) = arg.strip_prefix('-') {
                let key = stripped.to_string();
                if i + 1 < self.args.len() && !self.args[i + 1].starts_with('-') {
                    self.options.insert(key, Some(self.args[i + 1].clone()));
                    i += 2;
                } else {
                    self.options.insert(key, None);
                    i += 1;
                }
            } else {
                self.positional.push(arg.clone());
                i += 1;
            }
        }
    }

    pub fn has_option(&self, name: &str) -> bool {
        self.options.contains_key(name)
    }

    pub fn get_option(&self, name: &str) -> Option<&String> {
        self.options.get(name).and_then(|v| v.as_ref())
    }

    pub fn get_option_or(&self, name: &str, default: &str) -> String {
        self.get_option(name)
            .cloned()
            .unwrap_or_else(|| default.to_string())
    }

    pub fn get_positional(&self, index: usize) -> Option<&String> {
        self.positional.get(index)
    }

    pub fn positional_args(&self) -> &[String] {
        &self.positional
    }

    pub fn all_options(&self) -> &HashMap<String, Option<String>> {
        &self.options
    }
}

pub struct EnvVars;

impl EnvVars {
    pub fn get(key: &str) -> Option<String> {
        std::env::var(key).ok()
    }

    pub fn get_or(key: &str, default: &str) -> String {
        std::env::var(key).unwrap_or_else(|_| default.to_string())
    }

    pub fn set(key: &str, value: &str) {
        std::env::set_var(key, value);
    }

    pub fn remove(key: &str) {
        std::env::remove_var(key);
    }

    pub fn all() -> HashMap<String, String> {
        std::env::vars().collect()
    }

    pub fn exists(key: &str) -> bool {
        std::env::var(key).is_ok()
    }
}

pub struct Compression;

impl Compression {
    pub fn gzip_compress(data: &[u8]) -> Vec<u8> {
        let mut result = Vec::new();
        result.extend_from_slice(&[0x1f, 0x8b]);
        result.push(0x08);
        result.push(0x00);
        result.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
        result.push(0x00);
        result.push(0xff);

        let compressed = Self::deflate_compress(data);
        result.extend_from_slice(&compressed);

        let crc = Self::crc32(data);
        result.extend_from_slice(&crc.to_le_bytes());
        result.extend_from_slice(&(data.len() as u32).to_le_bytes());

        result
    }

    pub fn gzip_decompress(data: &[u8]) -> Result<Vec<u8>, String> {
        if data.len() < 18 {
            return Err("Invalid gzip data".to_string());
        }
        if data[0] != 0x1f || data[1] != 0x8b {
            return Err("Invalid gzip magic number".to_string());
        }

        let compressed_data = &data[10..data.len() - 8];
        Self::deflate_decompress(compressed_data)
    }

    fn deflate_compress(data: &[u8]) -> Vec<u8> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < data.len() {
            let chunk_size = std::cmp::min(65535, data.len() - i);
            let is_last = i + chunk_size >= data.len();

            result.push(if is_last { 0x01 } else { 0x00 });
            result.extend_from_slice(&(chunk_size as u16).to_le_bytes());
            result.extend_from_slice(&(!chunk_size as u16).to_le_bytes());
            result.extend_from_slice(&data[i..i + chunk_size]);

            i += chunk_size;
        }

        result
    }

    fn deflate_decompress(data: &[u8]) -> Result<Vec<u8>, String> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < data.len() {
            if i + 5 > data.len() {
                break;
            }

            let _is_last = (data[i] & 0x01) != 0;
            let len = u16::from_le_bytes([data[i + 1], data[i + 2]]) as usize;
            let _nlen = u16::from_le_bytes([data[i + 3], data[i + 4]]);

            i += 5;
            if i + len > data.len() {
                return Err("Invalid deflate data".to_string());
            }

            result.extend_from_slice(&data[i..i + len]);
            i += len;
        }

        Ok(result)
    }

    fn crc32(data: &[u8]) -> u32 {
        let mut crc = 0xffffffff;
        for &byte in data {
            crc ^= byte as u32;
            for _ in 0..8 {
                if (crc & 1) != 0 {
                    crc = (crc >> 1) ^ 0xedb88320;
                } else {
                    crc >>= 1;
                }
            }
        }
        !crc
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
        assert!(LogLevel::Error < LogLevel::Fatal);
    }

    #[test]
    fn test_logger_creation() {
        let logger = Logger::new(LogLevel::Info);
        logger.info("Test message");
        logger.debug("This should not appear");
    }

    #[test]
    fn test_logger_levels() {
        let logger = Logger::new(LogLevel::Warn);
        logger.trace("trace");
        logger.debug("debug");
        logger.info("info");
        logger.warn("warn");
        logger.error("error");
    }

    #[test]
    fn test_uuid_new_v4() {
        let uuid = Uuid::new_v4();
        assert!(!uuid.is_nil());
        assert_eq!(uuid.bytes[6] & 0xf0, 0x40);
        assert_eq!(uuid.bytes[8] & 0xc0, 0x80);
    }

    #[test]
    fn test_uuid_nil() {
        let uuid = Uuid::nil();
        assert!(uuid.is_nil());
        assert_eq!(uuid.to_string(), "00000000-0000-0000-0000-000000000000");
    }

    #[test]
    fn test_uuid_to_string() {
        let uuid = Uuid::from_bytes([
            0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc,
            0xde, 0xf0,
        ]);
        assert_eq!(uuid.as_hyphenated(), "12345678-9abc-def0-1234-56789abcdef0");
    }

    #[test]
    fn test_uuid_parse() {
        let uuid_str = "12345678-9abc-def0-1234-56789abcdef0";
        let uuid = Uuid::parse(uuid_str).unwrap();
        assert_eq!(uuid.to_string(), uuid_str);
    }

    #[test]
    fn test_uuid_parse_invalid() {
        assert!(Uuid::parse("invalid").is_err());
        assert!(Uuid::parse("12345678-9abc-def0-1234").is_err());
    }

    #[test]
    fn test_cli_parser_positional() {
        let args = vec![
            "program".to_string(),
            "arg1".to_string(),
            "arg2".to_string(),
        ];
        let parser = CliParser::new(args);
        assert_eq!(parser.get_positional(0), Some(&"arg1".to_string()));
        assert_eq!(parser.get_positional(1), Some(&"arg2".to_string()));
        assert_eq!(parser.get_positional(2), None);
    }

    #[test]
    fn test_cli_parser_options() {
        let args = vec![
            "program".to_string(),
            "--name".to_string(),
            "value".to_string(),
            "-f".to_string(),
        ];
        let parser = CliParser::new(args);
        assert!(parser.has_option("name"));
        assert_eq!(parser.get_option("name"), Some(&"value".to_string()));
        assert!(parser.has_option("f"));
        assert_eq!(parser.get_option("f"), None);
    }

    #[test]
    fn test_cli_parser_mixed() {
        let args = vec![
            "program".to_string(),
            "pos1".to_string(),
            "--opt1".to_string(),
            "val1".to_string(),
            "pos2".to_string(),
            "-f".to_string(),
        ];
        let parser = CliParser::new(args);
        assert_eq!(parser.positional_args().len(), 2);
        assert!(parser.has_option("opt1"));
        assert!(parser.has_option("f"));
    }

    #[test]
    fn test_env_vars() {
        EnvVars::set("TEST_VAR", "test_value");
        assert!(EnvVars::exists("TEST_VAR"));
        assert_eq!(EnvVars::get("TEST_VAR"), Some("test_value".to_string()));
        EnvVars::remove("TEST_VAR");
        assert!(!EnvVars::exists("TEST_VAR"));
    }

    #[test]
    fn test_env_vars_get_or() {
        let value = EnvVars::get_or("NONEXISTENT_VAR", "default");
        assert_eq!(value, "default");
    }

    #[test]
    fn test_compression_gzip() {
        let data = b"Hello, World!";
        let compressed = Compression::gzip_compress(data);
        assert!(!compressed.is_empty());
        assert_eq!(compressed[0], 0x1f);
        assert_eq!(compressed[1], 0x8b);
    }

    #[test]
    fn test_compression_gzip_roundtrip() {
        let data = b"The quick brown fox jumps over the lazy dog";
        let compressed = Compression::gzip_compress(data);
        let decompressed = Compression::gzip_decompress(&compressed).unwrap();
        assert_eq!(data.as_slice(), decompressed.as_slice());
    }

    #[test]
    fn test_compression_crc32() {
        let data = b"hello";
        let crc = Compression::crc32(data);
        assert!(crc > 0);
    }
}
