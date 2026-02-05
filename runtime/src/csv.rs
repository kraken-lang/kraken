//! CSV (Comma-Separated Values) parser and writer.
//!
//! Provides functionality for reading and writing CSV files.

#![allow(dead_code)]

use std::io::{BufRead, BufReader, Write};

/// CSV parser configuration
#[derive(Debug, Clone)]
pub struct CsvConfig {
    /// Field delimiter (default: ',')
    pub delimiter: char,
    /// Quote character (default: '"')
    pub quote: char,
    /// Whether the first row contains headers
    pub has_headers: bool,
    /// Whether to trim whitespace from fields
    pub trim_fields: bool,
}

impl Default for CsvConfig {
    fn default() -> Self {
        Self {
            delimiter: ',',
            quote: '"',
            has_headers: true,
            trim_fields: true,
        }
    }
}

/// CSV record representing a row of data
#[derive(Debug, Clone, PartialEq)]
pub struct CsvRecord {
    fields: Vec<String>,
}

impl CsvRecord {
    /// Create a new CSV record
    pub fn new(fields: Vec<String>) -> Self {
        Self { fields }
    }

    /// Get a field by index
    pub fn get(&self, index: usize) -> Option<&str> {
        self.fields.get(index).map(|s| s.as_str())
    }

    /// Get all fields
    pub fn fields(&self) -> &[String] {
        &self.fields
    }

    /// Number of fields in the record
    pub fn len(&self) -> usize {
        self.fields.len()
    }

    /// Check if the record is empty
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }
}

/// CSV parser
pub struct CsvParser {
    config: CsvConfig,
}

impl CsvParser {
    /// Create a new CSV parser with default configuration
    pub fn new() -> Self {
        Self {
            config: CsvConfig::default(),
        }
    }

    /// Create a new CSV parser with custom configuration
    pub fn with_config(config: CsvConfig) -> Self {
        Self { config }
    }

    /// Parse CSV data from a string
    pub fn parse(&self, data: &str) -> Result<Vec<CsvRecord>, String> {
        let mut records = Vec::new();
        let reader = BufReader::new(data.as_bytes());

        for (line_num, line) in reader.lines().enumerate() {
            let line = line.map_err(|e| format!("Failed to read line {line_num}: {e}"))?;

            if line.trim().is_empty() {
                continue;
            }

            let fields = self.parse_line(&line)?;
            records.push(CsvRecord::new(fields));
        }

        Ok(records)
    }

    fn parse_line(&self, line: &str) -> Result<Vec<String>, String> {
        let mut fields = Vec::new();
        let mut current_field = String::new();
        let mut in_quotes = false;
        let mut chars = line.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == self.config.quote {
                if in_quotes {
                    // Check for escaped quote
                    if chars.peek() == Some(&self.config.quote) {
                        current_field.push(self.config.quote);
                        chars.next();
                    } else {
                        in_quotes = false;
                    }
                } else {
                    in_quotes = true;
                }
            } else if ch == self.config.delimiter && !in_quotes {
                let field = if self.config.trim_fields {
                    current_field.trim().to_string()
                } else {
                    current_field.clone()
                };
                fields.push(field);
                current_field.clear();
            } else {
                current_field.push(ch);
            }
        }

        // Add the last field
        let field = if self.config.trim_fields {
            current_field.trim().to_string()
        } else {
            current_field
        };
        fields.push(field);

        Ok(fields)
    }

    /// Parse CSV data and return with headers
    pub fn parse_with_headers(&self, data: &str) -> Result<(Vec<String>, Vec<CsvRecord>), String> {
        let records = self.parse(data)?;

        if records.is_empty() {
            return Ok((Vec::new(), Vec::new()));
        }

        if self.config.has_headers {
            let headers = records[0].fields().to_vec();
            let data_records = records.into_iter().skip(1).collect();
            Ok((headers, data_records))
        } else {
            Ok((Vec::new(), records))
        }
    }
}

impl Default for CsvParser {
    fn default() -> Self {
        Self::new()
    }
}

/// CSV writer
pub struct CsvWriter {
    config: CsvConfig,
}

impl CsvWriter {
    /// Create a new CSV writer with default configuration
    pub fn new() -> Self {
        Self {
            config: CsvConfig::default(),
        }
    }

    /// Create a new CSV writer with custom configuration
    pub fn with_config(config: CsvConfig) -> Self {
        Self { config }
    }

    /// Write CSV records to a string
    pub fn write_to_string(&self, records: &[CsvRecord]) -> String {
        let mut output = String::new();

        for record in records {
            let line = self.format_record(record);
            output.push_str(&line);
            output.push('\n');
        }

        output
    }

    /// Write CSV records to a writer
    pub fn write<W: Write>(&self, writer: &mut W, records: &[CsvRecord]) -> Result<(), String> {
        for record in records {
            let line = self.format_record(record);
            writer
                .write_all(line.as_bytes())
                .map_err(|e| format!("Failed to write record: {e}"))?;
            writer
                .write_all(b"\n")
                .map_err(|e| format!("Failed to write newline: {e}"))?;
        }
        Ok(())
    }

    fn format_record(&self, record: &CsvRecord) -> String {
        record
            .fields()
            .iter()
            .map(|field| self.format_field(field))
            .collect::<Vec<_>>()
            .join(&self.config.delimiter.to_string())
    }

    fn format_field(&self, field: &str) -> String {
        if field.contains(self.config.delimiter)
            || field.contains(self.config.quote)
            || field.contains('\n')
        {
            let escaped = field.replace(
                &self.config.quote.to_string(),
                &format!("{}{}", self.config.quote, self.config.quote),
            );
            format!("{}{}{}", self.config.quote, escaped, self.config.quote)
        } else {
            field.to_string()
        }
    }
}

impl Default for CsvWriter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_csv() {
        let parser = CsvParser::new();
        let data = "name,age,city\nAlice,30,NYC\nBob,25,LA";
        let records = parser.parse(data).unwrap();

        assert_eq!(records.len(), 3);
        assert_eq!(records[0].get(0), Some("name"));
        assert_eq!(records[1].get(0), Some("Alice"));
        assert_eq!(records[2].get(0), Some("Bob"));
    }

    #[test]
    fn test_parse_with_headers() {
        let parser = CsvParser::new();
        let data = "name,age,city\nAlice,30,NYC\nBob,25,LA";
        let (headers, records) = parser.parse_with_headers(data).unwrap();

        assert_eq!(headers, vec!["name", "age", "city"]);
        assert_eq!(records.len(), 2);
        assert_eq!(records[0].get(0), Some("Alice"));
    }

    #[test]
    fn test_parse_quoted_fields() {
        let parser = CsvParser::new();
        let data = r#"name,description
"Alice","She said ""hello"""
"Bob","Simple text""#;
        let records = parser.parse(data).unwrap();

        assert_eq!(records.len(), 3);
        assert_eq!(records[1].get(0), Some("Alice"));
        assert_eq!(records[1].get(1), Some("She said \"hello\""));
    }

    #[test]
    fn test_parse_with_commas_in_quotes() {
        let parser = CsvParser::new();
        let data = r#"name,address
"Alice","123 Main St, Apt 4"
"Bob","456 Oak Ave""#;
        let records = parser.parse(data).unwrap();

        assert_eq!(records[1].get(1), Some("123 Main St, Apt 4"));
    }

    #[test]
    fn test_write_simple_csv() {
        let writer = CsvWriter::new();
        let records = vec![
            CsvRecord::new(vec!["name".to_string(), "age".to_string()]),
            CsvRecord::new(vec!["Alice".to_string(), "30".to_string()]),
            CsvRecord::new(vec!["Bob".to_string(), "25".to_string()]),
        ];

        let output = writer.write_to_string(&records);
        assert!(output.contains("name,age"));
        assert!(output.contains("Alice,30"));
        assert!(output.contains("Bob,25"));
    }

    #[test]
    fn test_write_with_quotes() {
        let writer = CsvWriter::new();
        let records = vec![
            CsvRecord::new(vec!["name".to_string(), "description".to_string()]),
            CsvRecord::new(vec!["Alice".to_string(), "She said \"hello\"".to_string()]),
        ];

        let output = writer.write_to_string(&records);
        assert!(output.contains(r#""She said ""hello""""#));
    }

    #[test]
    fn test_custom_delimiter() {
        let config = CsvConfig {
            delimiter: ';',
            ..Default::default()
        };
        let parser = CsvParser::with_config(config);
        let data = "name;age;city\nAlice;30;NYC";
        let records = parser.parse(data).unwrap();

        assert_eq!(records[1].get(0), Some("Alice"));
        assert_eq!(records[1].get(1), Some("30"));
    }

    #[test]
    fn test_trim_fields() {
        let parser = CsvParser::new();
        let data = "name , age , city\n Alice , 30 , NYC ";
        let records = parser.parse(data).unwrap();

        assert_eq!(records[0].get(0), Some("name"));
        assert_eq!(records[1].get(0), Some("Alice"));
    }

    #[test]
    fn test_empty_fields() {
        let parser = CsvParser::new();
        let data = "name,age,city\nAlice,,NYC\n,25,LA";
        let records = parser.parse(data).unwrap();

        assert_eq!(records[1].get(1), Some(""));
        assert_eq!(records[2].get(0), Some(""));
    }

    #[test]
    fn test_record_len() {
        let record = CsvRecord::new(vec!["a".to_string(), "b".to_string(), "c".to_string()]);
        assert_eq!(record.len(), 3);
        assert!(!record.is_empty());
    }
}
