//! Encoding and decoding utilities for the Kraken Language runtime.
//!
//! This module provides Base64, hexadecimal, and URL encoding/decoding functionality.

#![allow(dead_code)]

use std::fmt;

/// Error type for encoding/decoding operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EncodingError {
    InvalidInput(String),
    InvalidLength,
    InvalidCharacter(char),
}

impl fmt::Display for EncodingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EncodingError::InvalidInput(msg) => write!(f, "Invalid input: {msg}"),
            EncodingError::InvalidLength => write!(f, "Invalid length"),
            EncodingError::InvalidCharacter(c) => write!(f, "Invalid character: {c}"),
        }
    }
}

impl std::error::Error for EncodingError {}

/// Base64 encoding alphabet.
const BASE64_ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const BASE64_PAD: u8 = b'=';

/// Encodes bytes to Base64 string.
pub fn base64_encode(input: &[u8]) -> String {
    let mut result = Vec::new();
    let mut i = 0;

    while i + 2 < input.len() {
        let b1 = input[i];
        let b2 = input[i + 1];
        let b3 = input[i + 2];

        result.push(BASE64_ALPHABET[((b1 >> 2) & 0x3F) as usize]);
        result.push(BASE64_ALPHABET[(((b1 << 4) | (b2 >> 4)) & 0x3F) as usize]);
        result.push(BASE64_ALPHABET[(((b2 << 2) | (b3 >> 6)) & 0x3F) as usize]);
        result.push(BASE64_ALPHABET[(b3 & 0x3F) as usize]);

        i += 3;
    }

    // Handle remaining bytes
    if i < input.len() {
        let b1 = input[i];
        result.push(BASE64_ALPHABET[((b1 >> 2) & 0x3F) as usize]);

        if i + 1 < input.len() {
            let b2 = input[i + 1];
            result.push(BASE64_ALPHABET[(((b1 << 4) | (b2 >> 4)) & 0x3F) as usize]);
            result.push(BASE64_ALPHABET[((b2 << 2) & 0x3F) as usize]);
            result.push(BASE64_PAD);
        } else {
            result.push(BASE64_ALPHABET[((b1 << 4) & 0x3F) as usize]);
            result.push(BASE64_PAD);
            result.push(BASE64_PAD);
        }
    }

    String::from_utf8(result).unwrap()
}

/// Decodes Base64 string to bytes.
pub fn base64_decode(input: &str) -> Result<Vec<u8>, EncodingError> {
    let input = input.as_bytes();
    let mut result = Vec::new();
    let mut i = 0;

    // Create reverse lookup table
    let mut decode_table = [0xFF; 256];
    for (idx, &byte) in BASE64_ALPHABET.iter().enumerate() {
        decode_table[byte as usize] = idx as u8;
    }

    while i < input.len() {
        // Skip whitespace
        if input[i].is_ascii_whitespace() {
            i += 1;
            continue;
        }

        if i + 3 >= input.len() {
            break;
        }

        let b1 = decode_table[input[i] as usize];
        let b2 = decode_table[input[i + 1] as usize];
        let b3 = if input[i + 2] == BASE64_PAD {
            0
        } else {
            decode_table[input[i + 2] as usize]
        };
        let b4 = if input[i + 3] == BASE64_PAD {
            0
        } else {
            decode_table[input[i + 3] as usize]
        };

        if b1 == 0xFF || b2 == 0xFF {
            return Err(EncodingError::InvalidInput(
                "Invalid Base64 character".to_string(),
            ));
        }

        result.push((b1 << 2) | (b2 >> 4));

        if input[i + 2] != BASE64_PAD {
            result.push((b2 << 4) | (b3 >> 2));
        }

        if input[i + 3] != BASE64_PAD {
            result.push((b3 << 6) | b4);
        }

        i += 4;
    }

    Ok(result)
}

/// Encodes bytes to hexadecimal string.
pub fn hex_encode(input: &[u8]) -> String {
    const HEX_CHARS: &[u8] = b"0123456789abcdef";
    let mut result = Vec::with_capacity(input.len() * 2);

    for &byte in input {
        result.push(HEX_CHARS[(byte >> 4) as usize]);
        result.push(HEX_CHARS[(byte & 0x0F) as usize]);
    }

    String::from_utf8(result).unwrap()
}

/// Encodes bytes to uppercase hexadecimal string.
pub fn hex_encode_upper(input: &[u8]) -> String {
    const HEX_CHARS: &[u8] = b"0123456789ABCDEF";
    let mut result = Vec::with_capacity(input.len() * 2);

    for &byte in input {
        result.push(HEX_CHARS[(byte >> 4) as usize]);
        result.push(HEX_CHARS[(byte & 0x0F) as usize]);
    }

    String::from_utf8(result).unwrap()
}

/// Decodes hexadecimal string to bytes.
pub fn hex_decode(input: &str) -> Result<Vec<u8>, EncodingError> {
    let input = input.as_bytes();

    if input.len() % 2 != 0 {
        return Err(EncodingError::InvalidLength);
    }

    let mut result = Vec::with_capacity(input.len() / 2);

    for chunk in input.chunks(2) {
        let high = hex_char_to_value(chunk[0] as char)?;
        let low = hex_char_to_value(chunk[1] as char)?;
        result.push((high << 4) | low);
    }

    Ok(result)
}

/// Converts a hex character to its numeric value.
fn hex_char_to_value(c: char) -> Result<u8, EncodingError> {
    match c {
        '0'..='9' => Ok(c as u8 - b'0'),
        'a'..='f' => Ok(c as u8 - b'a' + 10),
        'A'..='F' => Ok(c as u8 - b'A' + 10),
        _ => Err(EncodingError::InvalidCharacter(c)),
    }
}

/// Encodes a string for use in URLs (percent encoding).
pub fn url_encode(input: &str) -> String {
    let mut result = String::new();

    for byte in input.bytes() {
        match byte {
            // Unreserved characters (RFC 3986)
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                result.push(byte as char);
            }
            // Space becomes +
            b' ' => result.push('+'),
            // Everything else is percent-encoded
            _ => {
                result.push('%');
                result.push_str(&format!("{byte:02X}"));
            }
        }
    }

    result
}

/// Decodes a URL-encoded string.
pub fn url_decode(input: &str) -> Result<String, EncodingError> {
    let mut result = Vec::new();
    let bytes = input.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        match bytes[i] {
            b'%' => {
                if i + 2 >= bytes.len() {
                    return Err(EncodingError::InvalidInput(
                        "Incomplete percent encoding".to_string(),
                    ));
                }

                let high = hex_char_to_value(bytes[i + 1] as char)?;
                let low = hex_char_to_value(bytes[i + 2] as char)?;
                result.push((high << 4) | low);
                i += 3;
            }
            b'+' => {
                result.push(b' ');
                i += 1;
            }
            byte => {
                result.push(byte);
                i += 1;
            }
        }
    }

    String::from_utf8(result).map_err(|_| EncodingError::InvalidInput("Invalid UTF-8".to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base64_encode_empty() {
        assert_eq!(base64_encode(b""), "");
    }

    #[test]
    fn test_base64_encode_basic() {
        assert_eq!(base64_encode(b"hello"), "aGVsbG8=");
        assert_eq!(base64_encode(b"hello world"), "aGVsbG8gd29ybGQ=");
    }

    #[test]
    fn test_base64_encode_padding() {
        assert_eq!(base64_encode(b"a"), "YQ==");
        assert_eq!(base64_encode(b"ab"), "YWI=");
        assert_eq!(base64_encode(b"abc"), "YWJj");
    }

    #[test]
    fn test_base64_decode_basic() {
        assert_eq!(base64_decode("aGVsbG8=").unwrap(), b"hello");
        assert_eq!(base64_decode("aGVsbG8gd29ybGQ=").unwrap(), b"hello world");
    }

    #[test]
    fn test_base64_roundtrip() {
        let data = b"The quick brown fox jumps over the lazy dog";
        let encoded = base64_encode(data);
        let decoded = base64_decode(&encoded).unwrap();
        assert_eq!(decoded, data);
    }

    #[test]
    fn test_hex_encode_empty() {
        assert_eq!(hex_encode(b""), "");
    }

    #[test]
    fn test_hex_encode_basic() {
        assert_eq!(hex_encode(b"hello"), "68656c6c6f");
        assert_eq!(hex_encode(&[0, 15, 255]), "000fff");
    }

    #[test]
    fn test_hex_encode_upper() {
        assert_eq!(hex_encode_upper(b"hello"), "68656C6C6F");
        assert_eq!(hex_encode_upper(&[0, 15, 255]), "000FFF");
    }

    #[test]
    fn test_hex_decode_basic() {
        assert_eq!(hex_decode("68656c6c6f").unwrap(), b"hello");
        assert_eq!(hex_decode("000fff").unwrap(), vec![0, 15, 255]);
    }

    #[test]
    fn test_hex_decode_uppercase() {
        assert_eq!(hex_decode("68656C6C6F").unwrap(), b"hello");
        assert_eq!(hex_decode("000FFF").unwrap(), vec![0, 15, 255]);
    }

    #[test]
    fn test_hex_decode_mixed_case() {
        assert_eq!(hex_decode("68656C6c6F").unwrap(), b"hello");
    }

    #[test]
    fn test_hex_decode_invalid_length() {
        assert!(hex_decode("abc").is_err());
    }

    #[test]
    fn test_hex_decode_invalid_char() {
        assert!(hex_decode("xyz").is_err());
    }

    #[test]
    fn test_hex_roundtrip() {
        let data = b"The quick brown fox";
        let encoded = hex_encode(data);
        let decoded = hex_decode(&encoded).unwrap();
        assert_eq!(decoded, data);
    }

    #[test]
    fn test_url_encode_basic() {
        assert_eq!(url_encode("hello world"), "hello+world");
        assert_eq!(url_encode("hello@world"), "hello%40world");
    }

    #[test]
    fn test_url_encode_special_chars() {
        assert_eq!(url_encode("a+b=c&d"), "a%2Bb%3Dc%26d");
        assert_eq!(url_encode("100%"), "100%25");
    }

    #[test]
    fn test_url_encode_unreserved() {
        assert_eq!(url_encode("abc-123_XYZ.~"), "abc-123_XYZ.~");
    }

    #[test]
    fn test_url_decode_basic() {
        assert_eq!(url_decode("hello+world").unwrap(), "hello world");
        assert_eq!(url_decode("hello%40world").unwrap(), "hello@world");
    }

    #[test]
    fn test_url_decode_special_chars() {
        assert_eq!(url_decode("a%2Bb%3Dc%26d").unwrap(), "a+b=c&d");
        assert_eq!(url_decode("100%25").unwrap(), "100%");
    }

    #[test]
    fn test_url_roundtrip() {
        let data = "hello world! @#$%^&*()";
        let encoded = url_encode(data);
        let decoded = url_decode(&encoded).unwrap();
        assert_eq!(decoded, data);
    }

    #[test]
    fn test_url_decode_invalid_percent() {
        assert!(url_decode("hello%").is_err());
        assert!(url_decode("hello%2").is_err());
    }

    #[test]
    fn test_url_decode_invalid_hex() {
        assert!(url_decode("hello%ZZ").is_err());
    }
}
