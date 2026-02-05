//! Comprehensive formatting module for the Kraken Language runtime.
//!
//! This module provides formatting utilities similar to Rust's std::fmt,
//! including support for various display formats, padding, alignment, and precision.

#![allow(dead_code)]

use std::fmt::{self, Write as FmtWrite};

/// Alignment options for formatted output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Alignment {
    Left,
    Right,
    Center,
}

/// Formatting specification for a value.
#[derive(Debug, Clone)]
pub struct FormatSpec {
    pub fill: char,
    pub align: Option<Alignment>,
    pub sign: Option<char>,
    pub alternate: bool,
    pub zero_pad: bool,
    pub width: Option<usize>,
    pub precision: Option<usize>,
    pub type_spec: Option<char>,
}

impl Default for FormatSpec {
    fn default() -> Self {
        FormatSpec {
            fill: ' ',
            align: None,
            sign: None,
            alternate: false,
            zero_pad: false,
            width: None,
            precision: None,
            type_spec: None,
        }
    }
}

impl FormatSpec {
    /// Creates a new format specification with default values.
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the fill character for padding.
    pub fn with_fill(mut self, fill: char) -> Self {
        self.fill = fill;
        self
    }

    /// Sets the alignment.
    pub fn with_align(mut self, align: Alignment) -> Self {
        self.align = Some(align);
        self
    }

    /// Sets the width.
    pub fn with_width(mut self, width: usize) -> Self {
        self.width = Some(width);
        self
    }

    /// Sets the precision.
    pub fn with_precision(mut self, precision: usize) -> Self {
        self.precision = Some(precision);
        self
    }

    /// Sets zero padding.
    pub fn with_zero_pad(mut self) -> Self {
        self.zero_pad = true;
        self
    }

    /// Sets the alternate format flag.
    pub fn with_alternate(mut self) -> Self {
        self.alternate = true;
        self
    }
}

/// A trait for types that can be formatted.
pub trait Display {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result;
}

/// A trait for types that can be formatted for debugging.
pub trait Debug {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result;
}

/// A formatter for writing formatted output.
pub struct Formatter {
    buf: String,
    spec: FormatSpec,
}

impl Formatter {
    /// Creates a new formatter with default formatting.
    pub fn new() -> Self {
        Formatter {
            buf: String::new(),
            spec: FormatSpec::default(),
        }
    }

    /// Creates a new formatter with the given format specification.
    pub fn with_spec(spec: FormatSpec) -> Self {
        Formatter {
            buf: String::new(),
            spec,
        }
    }

    /// Writes a string to the formatter.
    pub fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buf.push_str(s);
        Ok(())
    }

    /// Writes a formatted string to the formatter.
    pub fn write_fmt(&mut self, args: fmt::Arguments) -> fmt::Result {
        fmt::write(self, args)
    }

    /// Returns the formatted output as a string.
    pub fn finish(self) -> String {
        self.buf
    }

    /// Pads a string according to the format specification.
    pub fn pad(&mut self, s: &str) -> fmt::Result {
        let width = self.spec.width.unwrap_or(0);
        let len = s.len();

        if len >= width {
            return self.write_str(s);
        }

        let padding = width - len;
        let align = self.spec.align.unwrap_or(Alignment::Left);

        match align {
            Alignment::Left => {
                self.write_str(s)?;
                for _ in 0..padding {
                    self.buf.push(self.spec.fill);
                }
            }
            Alignment::Right => {
                for _ in 0..padding {
                    self.buf.push(self.spec.fill);
                }
                self.write_str(s)?;
            }
            Alignment::Center => {
                let left_pad = padding / 2;
                let right_pad = padding - left_pad;
                for _ in 0..left_pad {
                    self.buf.push(self.spec.fill);
                }
                self.write_str(s)?;
                for _ in 0..right_pad {
                    self.buf.push(self.spec.fill);
                }
            }
        }

        Ok(())
    }
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
    }
}

impl FmtWrite for Formatter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buf.push_str(s);
        Ok(())
    }
}

/// Formats an integer with the given specification.
pub fn format_int(value: i64, spec: &FormatSpec) -> String {
    let mut result = String::new();

    // Handle sign
    if value < 0 {
        result.push('-');
    } else if let Some(sign) = spec.sign {
        result.push(sign);
    }

    // Format the absolute value
    let abs_value = value.abs();
    let formatted = if spec.alternate && spec.type_spec == Some('x') {
        format!("0x{abs_value:x}")
    } else if spec.alternate && spec.type_spec == Some('X') {
        format!("0X{abs_value:X}")
    } else if spec.alternate && spec.type_spec == Some('o') {
        format!("0o{abs_value:o}")
    } else if spec.alternate && spec.type_spec == Some('b') {
        format!("0b{abs_value:b}")
    } else if spec.type_spec == Some('x') {
        format!("{abs_value:x}")
    } else if spec.type_spec == Some('X') {
        format!("{abs_value:X}")
    } else if spec.type_spec == Some('o') {
        format!("{abs_value:o}")
    } else if spec.type_spec == Some('b') {
        format!("{abs_value:b}")
    } else {
        abs_value.to_string()
    };

    result.push_str(&formatted);

    // Apply padding
    if let Some(width) = spec.width {
        let len = result.len();
        if len < width {
            let padding = width - len;
            let align = spec.align.unwrap_or(Alignment::Right);

            match align {
                Alignment::Left => {
                    for _ in 0..padding {
                        result.push(spec.fill);
                    }
                }
                Alignment::Right => {
                    let mut padded = String::new();
                    for _ in 0..padding {
                        padded.push(spec.fill);
                    }
                    padded.push_str(&result);
                    result = padded;
                }
                Alignment::Center => {
                    let left_pad = padding / 2;
                    let right_pad = padding - left_pad;
                    let mut padded = String::new();
                    for _ in 0..left_pad {
                        padded.push(spec.fill);
                    }
                    padded.push_str(&result);
                    for _ in 0..right_pad {
                        padded.push(spec.fill);
                    }
                    result = padded;
                }
            }
        }
    }

    result
}

/// Formats a float with the given specification.
pub fn format_float(value: f64, spec: &FormatSpec) -> String {
    let precision = spec.precision.unwrap_or(6);
    let formatted = format!("{value:.precision$}");

    // Apply padding
    if let Some(width) = spec.width {
        let len = formatted.len();
        if len < width {
            let padding = width - len;
            let align = spec.align.unwrap_or(Alignment::Right);
            let mut result = String::new();

            match align {
                Alignment::Left => {
                    result.push_str(&formatted);
                    for _ in 0..padding {
                        result.push(spec.fill);
                    }
                }
                Alignment::Right => {
                    for _ in 0..padding {
                        result.push(spec.fill);
                    }
                    result.push_str(&formatted);
                }
                Alignment::Center => {
                    let left_pad = padding / 2;
                    let right_pad = padding - left_pad;
                    for _ in 0..left_pad {
                        result.push(spec.fill);
                    }
                    result.push_str(&formatted);
                    for _ in 0..right_pad {
                        result.push(spec.fill);
                    }
                }
            }

            return result;
        }
    }

    formatted
}

/// Formats a string with the given specification.
pub fn format_string(value: &str, spec: &FormatSpec) -> String {
    let s = if let Some(precision) = spec.precision {
        &value[..precision.min(value.len())]
    } else {
        value
    };

    // Apply padding
    if let Some(width) = spec.width {
        let len = s.len();
        if len < width {
            let padding = width - len;
            let align = spec.align.unwrap_or(Alignment::Left);
            let mut result = String::new();

            match align {
                Alignment::Left => {
                    result.push_str(s);
                    for _ in 0..padding {
                        result.push(spec.fill);
                    }
                }
                Alignment::Right => {
                    for _ in 0..padding {
                        result.push(spec.fill);
                    }
                    result.push_str(s);
                }
                Alignment::Center => {
                    let left_pad = padding / 2;
                    let right_pad = padding - left_pad;
                    for _ in 0..left_pad {
                        result.push(spec.fill);
                    }
                    result.push_str(s);
                    for _ in 0..right_pad {
                        result.push(spec.fill);
                    }
                }
            }

            return result;
        }
    }

    s.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_spec_default() {
        let spec = FormatSpec::default();
        assert_eq!(spec.fill, ' ');
        assert_eq!(spec.align, None);
        assert_eq!(spec.width, None);
    }

    #[test]
    fn test_format_spec_builder() {
        let spec = FormatSpec::new()
            .with_fill('0')
            .with_align(Alignment::Right)
            .with_width(10)
            .with_precision(2);

        assert_eq!(spec.fill, '0');
        assert_eq!(spec.align, Some(Alignment::Right));
        assert_eq!(spec.width, Some(10));
        assert_eq!(spec.precision, Some(2));
    }

    #[test]
    fn test_format_int_basic() {
        let spec = FormatSpec::default();
        assert_eq!(format_int(42, &spec), "42");
        assert_eq!(format_int(-42, &spec), "-42");
    }

    #[test]
    fn test_format_int_hex() {
        let mut spec = FormatSpec {
            type_spec: Some('x'),
            ..Default::default()
        };
        assert_eq!(format_int(255, &spec), "ff");

        spec.alternate = true;
        assert_eq!(format_int(255, &spec), "0xff");
    }

    #[test]
    fn test_format_int_binary() {
        let mut spec = FormatSpec {
            type_spec: Some('b'),
            ..Default::default()
        };
        assert_eq!(format_int(5, &spec), "101");

        spec.alternate = true;
        assert_eq!(format_int(5, &spec), "0b101");
    }

    #[test]
    fn test_format_int_octal() {
        let mut spec = FormatSpec {
            type_spec: Some('o'),
            ..Default::default()
        };
        assert_eq!(format_int(8, &spec), "10");

        spec.alternate = true;
        assert_eq!(format_int(8, &spec), "0o10");
    }

    #[test]
    fn test_format_int_width() {
        let spec = FormatSpec::new().with_width(5);
        assert_eq!(format_int(42, &spec), "   42");
    }

    #[test]
    fn test_format_int_zero_pad() {
        let spec = FormatSpec::new().with_width(5).with_fill('0');
        assert_eq!(format_int(42, &spec), "00042");
    }

    #[test]
    fn test_format_float_basic() {
        let spec = FormatSpec::default();
        assert_eq!(format_float(2.71828, &spec), "2.718280");
    }

    #[test]
    fn test_format_float_precision() {
        let spec = FormatSpec::new().with_precision(2);
        assert_eq!(format_float(2.71828, &spec), "2.72");
    }

    #[test]
    fn test_format_float_width() {
        let spec = FormatSpec::new().with_width(10).with_precision(2);
        assert_eq!(format_float(2.718, &spec), "      2.72");
    }

    #[test]
    fn test_format_string_basic() {
        let spec = FormatSpec::default();
        assert_eq!(format_string("hello", &spec), "hello");
    }

    #[test]
    fn test_format_string_width_left() {
        let spec = FormatSpec::new().with_width(10).with_align(Alignment::Left);
        assert_eq!(format_string("hello", &spec), "hello     ");
    }

    #[test]
    fn test_format_string_width_right() {
        let spec = FormatSpec::new()
            .with_width(10)
            .with_align(Alignment::Right);
        assert_eq!(format_string("hello", &spec), "     hello");
    }

    #[test]
    fn test_format_string_width_center() {
        let spec = FormatSpec::new()
            .with_width(10)
            .with_align(Alignment::Center);
        assert_eq!(format_string("hello", &spec), "  hello   ");
    }

    #[test]
    fn test_format_string_precision() {
        let spec = FormatSpec::new().with_precision(3);
        assert_eq!(format_string("hello", &spec), "hel");
    }

    #[test]
    fn test_formatter_basic() {
        let mut f = Formatter::new();
        f.write_str("hello").unwrap();
        assert_eq!(f.finish(), "hello");
    }

    #[test]
    fn test_formatter_pad_left() {
        let spec = FormatSpec::new().with_width(10).with_align(Alignment::Left);
        let mut f = Formatter::with_spec(spec);
        f.pad("hello").unwrap();
        assert_eq!(f.finish(), "hello     ");
    }

    #[test]
    fn test_formatter_pad_right() {
        let spec = FormatSpec::new()
            .with_width(10)
            .with_align(Alignment::Right);
        let mut f = Formatter::with_spec(spec);
        f.pad("hello").unwrap();
        assert_eq!(f.finish(), "     hello");
    }

    #[test]
    fn test_formatter_pad_center() {
        let spec = FormatSpec::new()
            .with_width(10)
            .with_align(Alignment::Center);
        let mut f = Formatter::with_spec(spec);
        f.pad("hello").unwrap();
        assert_eq!(f.finish(), "  hello   ");
    }
}
