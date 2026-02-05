//! String encoding utilities for UTF-16, UTF-32, and ASCII conversions.

#![allow(dead_code)]

use std::char;

/// String encoding utilities
pub struct StringEncoder;

impl StringEncoder {
    /// Encode a string to UTF-16 (little-endian)
    pub fn to_utf16(text: &str) -> Vec<u16> {
        text.encode_utf16().collect()
    }

    /// Decode UTF-16 (little-endian) to a string
    pub fn from_utf16(data: &[u16]) -> Result<String, String> {
        String::from_utf16(data).map_err(|e| e.to_string())
    }

    /// Encode a string to UTF-16 bytes (little-endian)
    pub fn to_utf16_bytes(text: &str) -> Vec<u8> {
        let utf16: Vec<u16> = text.encode_utf16().collect();
        let mut bytes = Vec::with_capacity(utf16.len() * 2);
        for code_unit in utf16 {
            bytes.push((code_unit & 0xFF) as u8);
            bytes.push((code_unit >> 8) as u8);
        }
        bytes
    }

    /// Decode UTF-16 bytes (little-endian) to a string
    pub fn from_utf16_bytes(bytes: &[u8]) -> Result<String, String> {
        if !bytes.len().is_multiple_of(2) {
            return Err("Invalid UTF-16 byte sequence: odd length".to_string());
        }

        let mut utf16 = Vec::with_capacity(bytes.len() / 2);
        for chunk in bytes.chunks_exact(2) {
            let code_unit = u16::from_le_bytes([chunk[0], chunk[1]]);
            utf16.push(code_unit);
        }

        Self::from_utf16(&utf16)
    }

    /// Encode a string to UTF-16 bytes (big-endian)
    pub fn to_utf16_be_bytes(text: &str) -> Vec<u8> {
        let utf16: Vec<u16> = text.encode_utf16().collect();
        let mut bytes = Vec::with_capacity(utf16.len() * 2);
        for code_unit in utf16 {
            bytes.push((code_unit >> 8) as u8);
            bytes.push((code_unit & 0xFF) as u8);
        }
        bytes
    }

    /// Decode UTF-16 bytes (big-endian) to a string
    pub fn from_utf16_be_bytes(bytes: &[u8]) -> Result<String, String> {
        if !bytes.len().is_multiple_of(2) {
            return Err("Invalid UTF-16 byte sequence: odd length".to_string());
        }

        let mut utf16 = Vec::with_capacity(bytes.len() / 2);
        for chunk in bytes.chunks_exact(2) {
            let code_unit = u16::from_be_bytes([chunk[0], chunk[1]]);
            utf16.push(code_unit);
        }

        Self::from_utf16(&utf16)
    }

    /// Encode a string to UTF-32 code points
    pub fn to_utf32(text: &str) -> Vec<u32> {
        text.chars().map(|c| c as u32).collect()
    }

    /// Decode UTF-32 code points to a string
    pub fn from_utf32(data: &[u32]) -> Result<String, String> {
        let mut result = String::new();
        for &code_point in data {
            match char::from_u32(code_point) {
                Some(c) => result.push(c),
                None => return Err(format!("Invalid UTF-32 code point: {code_point}")),
            }
        }
        Ok(result)
    }

    /// Encode a string to UTF-32 bytes (little-endian)
    pub fn to_utf32_bytes(text: &str) -> Vec<u8> {
        let utf32 = Self::to_utf32(text);
        let mut bytes = Vec::with_capacity(utf32.len() * 4);
        for code_point in utf32 {
            bytes.extend_from_slice(&code_point.to_le_bytes());
        }
        bytes
    }

    /// Decode UTF-32 bytes (little-endian) to a string
    pub fn from_utf32_bytes(bytes: &[u8]) -> Result<String, String> {
        if !bytes.len().is_multiple_of(4) {
            return Err("Invalid UTF-32 byte sequence: length not multiple of 4".to_string());
        }

        let mut utf32 = Vec::with_capacity(bytes.len() / 4);
        for chunk in bytes.chunks_exact(4) {
            let code_point = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
            utf32.push(code_point);
        }

        Self::from_utf32(&utf32)
    }

    /// Encode a string to UTF-32 bytes (big-endian)
    pub fn to_utf32_be_bytes(text: &str) -> Vec<u8> {
        let utf32 = Self::to_utf32(text);
        let mut bytes = Vec::with_capacity(utf32.len() * 4);
        for code_point in utf32 {
            bytes.extend_from_slice(&code_point.to_be_bytes());
        }
        bytes
    }

    /// Decode UTF-32 bytes (big-endian) to a string
    pub fn from_utf32_be_bytes(bytes: &[u8]) -> Result<String, String> {
        if !bytes.len().is_multiple_of(4) {
            return Err("Invalid UTF-32 byte sequence: length not multiple of 4".to_string());
        }

        let mut utf32 = Vec::with_capacity(bytes.len() / 4);
        for chunk in bytes.chunks_exact(4) {
            let code_point = u32::from_be_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
            utf32.push(code_point);
        }

        Self::from_utf32(&utf32)
    }

    /// Convert a string to ASCII, replacing non-ASCII characters with '?'
    pub fn to_ascii_lossy(text: &str) -> String {
        text.chars()
            .map(|c| if c.is_ascii() { c } else { '?' })
            .collect()
    }

    /// Convert a string to ASCII, returning error if non-ASCII characters exist
    pub fn to_ascii(text: &str) -> Result<String, String> {
        if text.is_ascii() {
            Ok(text.to_string())
        } else {
            Err("String contains non-ASCII characters".to_string())
        }
    }

    /// Convert ASCII bytes to a string
    pub fn from_ascii(bytes: &[u8]) -> Result<String, String> {
        if bytes.iter().all(|&b| b.is_ascii()) {
            Ok(String::from_utf8_lossy(bytes).to_string())
        } else {
            Err("Byte sequence contains non-ASCII bytes".to_string())
        }
    }

    /// Check if a string is valid ASCII
    pub fn is_ascii(text: &str) -> bool {
        text.is_ascii()
    }

    /// Get the byte length of a string in UTF-8
    pub fn utf8_len(text: &str) -> usize {
        text.len()
    }

    /// Get the character count of a string
    pub fn char_count(text: &str) -> usize {
        text.chars().count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_utf16_encode_decode() {
        let text = "Hello, 世界! 🦀";
        let utf16 = StringEncoder::to_utf16(text);
        let decoded = StringEncoder::from_utf16(&utf16).unwrap();
        assert_eq!(text, decoded);
    }

    #[test]
    fn test_utf16_bytes_le() {
        let text = "Hello";
        let bytes = StringEncoder::to_utf16_bytes(text);
        let decoded = StringEncoder::from_utf16_bytes(&bytes).unwrap();
        assert_eq!(text, decoded);
    }

    #[test]
    fn test_utf16_bytes_be() {
        let text = "Hello";
        let bytes = StringEncoder::to_utf16_be_bytes(text);
        let decoded = StringEncoder::from_utf16_be_bytes(&bytes).unwrap();
        assert_eq!(text, decoded);
    }

    #[test]
    fn test_utf16_bytes_invalid_length() {
        let bytes = vec![0x48, 0x00, 0x65];
        let result = StringEncoder::from_utf16_bytes(&bytes);
        assert!(result.is_err());
    }

    #[test]
    fn test_utf32_encode_decode() {
        let text = "Hello, 世界! 🦀";
        let utf32 = StringEncoder::to_utf32(text);
        let decoded = StringEncoder::from_utf32(&utf32).unwrap();
        assert_eq!(text, decoded);
    }

    #[test]
    fn test_utf32_bytes_le() {
        let text = "Hello";
        let bytes = StringEncoder::to_utf32_bytes(text);
        let decoded = StringEncoder::from_utf32_bytes(&bytes).unwrap();
        assert_eq!(text, decoded);
    }

    #[test]
    fn test_utf32_bytes_be() {
        let text = "Hello";
        let bytes = StringEncoder::to_utf32_be_bytes(text);
        let decoded = StringEncoder::from_utf32_be_bytes(&bytes).unwrap();
        assert_eq!(text, decoded);
    }

    #[test]
    fn test_utf32_bytes_invalid_length() {
        let bytes = vec![0x48, 0x00, 0x00];
        let result = StringEncoder::from_utf32_bytes(&bytes);
        assert!(result.is_err());
    }

    #[test]
    fn test_utf32_invalid_code_point() {
        let invalid = vec![0xD800];
        let result = StringEncoder::from_utf32(&invalid);
        assert!(result.is_err());
    }

    #[test]
    fn test_ascii_conversion() {
        let text = "Hello";
        assert_eq!(StringEncoder::to_ascii(text).unwrap(), "Hello");
        assert!(StringEncoder::is_ascii(text));
    }

    #[test]
    fn test_ascii_lossy() {
        let text = "Hello, 世界!";
        let ascii = StringEncoder::to_ascii_lossy(text);
        assert_eq!(ascii, "Hello, ??!");
    }

    #[test]
    fn test_ascii_non_ascii_error() {
        let text = "Hello, 世界!";
        let result = StringEncoder::to_ascii(text);
        assert!(result.is_err());
    }

    #[test]
    fn test_from_ascii() {
        let bytes = b"Hello";
        let result = StringEncoder::from_ascii(bytes).unwrap();
        assert_eq!(result, "Hello");
    }

    #[test]
    fn test_from_ascii_invalid() {
        let bytes = vec![0x48, 0x65, 0xFF, 0x6C, 0x6F];
        let result = StringEncoder::from_ascii(&bytes);
        assert!(result.is_err());
    }

    #[test]
    fn test_utf8_len() {
        let text = "Hello, 世界!";
        assert_eq!(StringEncoder::utf8_len(text), 14);
    }

    #[test]
    fn test_char_count() {
        let text = "Hello, 世界!";
        assert_eq!(StringEncoder::char_count(text), 10);
    }

    #[test]
    fn test_emoji_utf16() {
        let text = "🦀🎉";
        let utf16 = StringEncoder::to_utf16(text);
        let decoded = StringEncoder::from_utf16(&utf16).unwrap();
        assert_eq!(text, decoded);
    }

    #[test]
    fn test_emoji_utf32() {
        let text = "🦀🎉";
        let utf32 = StringEncoder::to_utf32(text);
        assert_eq!(utf32.len(), 2);
        let decoded = StringEncoder::from_utf32(&utf32).unwrap();
        assert_eq!(text, decoded);
    }
}
