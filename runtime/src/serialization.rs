//! Serialization and deserialization support for data interchange.

#![allow(dead_code)]

use std::collections::HashMap;

/// JSON value representation
#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

impl JsonValue {
    /// Check if value is null
    pub fn is_null(&self) -> bool {
        matches!(self, JsonValue::Null)
    }

    /// Check if value is boolean
    pub fn is_bool(&self) -> bool {
        matches!(self, JsonValue::Bool(_))
    }

    /// Check if value is number
    pub fn is_number(&self) -> bool {
        matches!(self, JsonValue::Number(_))
    }

    /// Check if value is string
    pub fn is_string(&self) -> bool {
        matches!(self, JsonValue::String(_))
    }

    /// Check if value is array
    pub fn is_array(&self) -> bool {
        matches!(self, JsonValue::Array(_))
    }

    /// Check if value is object
    pub fn is_object(&self) -> bool {
        matches!(self, JsonValue::Object(_))
    }

    /// Get as boolean
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            JsonValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Get as number
    pub fn as_number(&self) -> Option<f64> {
        match self {
            JsonValue::Number(n) => Some(*n),
            _ => None,
        }
    }

    /// Get as string
    pub fn as_string(&self) -> Option<&str> {
        match self {
            JsonValue::String(s) => Some(s),
            _ => None,
        }
    }

    /// Get as array
    pub fn as_array(&self) -> Option<&Vec<JsonValue>> {
        match self {
            JsonValue::Array(a) => Some(a),
            _ => None,
        }
    }

    /// Get as object
    pub fn as_object(&self) -> Option<&HashMap<String, JsonValue>> {
        match self {
            JsonValue::Object(o) => Some(o),
            _ => None,
        }
    }

    /// Serialize to JSON string
    pub fn serialize_json(&self) -> String {
        match self {
            JsonValue::Null => "null".to_string(),
            JsonValue::Bool(b) => b.to_string(),
            JsonValue::Number(n) => n.to_string(),
            JsonValue::String(s) => format!("\"{}\"", s.replace('\"', "\\\"")),
            JsonValue::Array(arr) => {
                let items: Vec<String> = arr.iter().map(|v| v.serialize_json()).collect();
                format!("[{}]", items.join(","))
            }
            JsonValue::Object(obj) => {
                let items: Vec<String> = obj
                    .iter()
                    .map(|(k, v)| format!("\"{}\":{}", k, v.serialize_json()))
                    .collect();
                format!("{{{}}}", items.join(","))
            }
        }
    }
}

/// JSON serializer
pub struct JsonSerializer;

impl JsonSerializer {
    /// Serialize value to JSON string
    pub fn serialize(value: &JsonValue) -> String {
        value.serialize_json()
    }

    /// Create null value
    pub fn null() -> JsonValue {
        JsonValue::Null
    }

    /// Create boolean value
    pub fn bool(value: bool) -> JsonValue {
        JsonValue::Bool(value)
    }

    /// Create number value
    pub fn number(value: f64) -> JsonValue {
        JsonValue::Number(value)
    }

    /// Create string value
    pub fn string(value: String) -> JsonValue {
        JsonValue::String(value)
    }

    /// Create array value
    pub fn array(values: Vec<JsonValue>) -> JsonValue {
        JsonValue::Array(values)
    }

    /// Create object value
    pub fn object(values: HashMap<String, JsonValue>) -> JsonValue {
        JsonValue::Object(values)
    }
}

/// Binary serialization format
pub struct BinarySerializer;

impl BinarySerializer {
    /// Serialize integer to bytes (little-endian)
    pub fn serialize_i32(value: i32) -> Vec<u8> {
        value.to_le_bytes().to_vec()
    }

    /// Serialize integer to bytes (little-endian)
    pub fn serialize_i64(value: i64) -> Vec<u8> {
        value.to_le_bytes().to_vec()
    }

    /// Serialize float to bytes (little-endian)
    pub fn serialize_f32(value: f32) -> Vec<u8> {
        value.to_le_bytes().to_vec()
    }

    /// Serialize float to bytes (little-endian)
    pub fn serialize_f64(value: f64) -> Vec<u8> {
        value.to_le_bytes().to_vec()
    }

    /// Deserialize i32 from bytes (little-endian)
    pub fn deserialize_i32(bytes: &[u8]) -> Option<i32> {
        if bytes.len() < 4 {
            return None;
        }
        let mut arr = [0u8; 4];
        arr.copy_from_slice(&bytes[..4]);
        Some(i32::from_le_bytes(arr))
    }

    /// Deserialize i64 from bytes (little-endian)
    pub fn deserialize_i64(bytes: &[u8]) -> Option<i64> {
        if bytes.len() < 8 {
            return None;
        }
        let mut arr = [0u8; 8];
        arr.copy_from_slice(&bytes[..8]);
        Some(i64::from_le_bytes(arr))
    }

    /// Deserialize f32 from bytes (little-endian)
    pub fn deserialize_f32(bytes: &[u8]) -> Option<f32> {
        if bytes.len() < 4 {
            return None;
        }
        let mut arr = [0u8; 4];
        arr.copy_from_slice(&bytes[..4]);
        Some(f32::from_le_bytes(arr))
    }

    /// Deserialize f64 from bytes (little-endian)
    pub fn deserialize_f64(bytes: &[u8]) -> Option<f64> {
        if bytes.len() < 8 {
            return None;
        }
        let mut arr = [0u8; 8];
        arr.copy_from_slice(&bytes[..8]);
        Some(f64::from_le_bytes(arr))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_null() {
        let value = JsonValue::Null;
        assert!(value.is_null());
        assert_eq!(value.serialize_json(), "null");
    }

    #[test]
    fn test_json_bool() {
        let value = JsonValue::Bool(true);
        assert!(value.is_bool());
        assert_eq!(value.as_bool(), Some(true));
        assert_eq!(value.serialize_json(), "true");
    }

    #[test]
    fn test_json_number() {
        let value = JsonValue::Number(42.5);
        assert!(value.is_number());
        assert_eq!(value.as_number(), Some(42.5));
        assert_eq!(value.serialize_json(), "42.5");
    }

    #[test]
    fn test_json_string() {
        let value = JsonValue::String("hello".to_string());
        assert!(value.is_string());
        assert_eq!(value.as_string(), Some("hello"));
        assert_eq!(value.serialize_json(), "\"hello\"");
    }

    #[test]
    fn test_json_array() {
        let value = JsonValue::Array(vec![
            JsonValue::Number(1.0),
            JsonValue::Number(2.0),
            JsonValue::Number(3.0),
        ]);
        assert!(value.is_array());
        assert_eq!(value.serialize_json(), "[1,2,3]");
    }

    #[test]
    fn test_json_object() {
        let mut obj = HashMap::new();
        obj.insert("name".to_string(), JsonValue::String("test".to_string()));
        obj.insert("age".to_string(), JsonValue::Number(25.0));

        let value = JsonValue::Object(obj);
        assert!(value.is_object());
        assert!(value.serialize_json().contains("\"name\":\"test\""));
    }

    #[test]
    fn test_json_serializer() {
        let value = JsonSerializer::object({
            let mut map = HashMap::new();
            map.insert(
                "key".to_string(),
                JsonSerializer::string("value".to_string()),
            );
            map
        });

        let json = JsonSerializer::serialize(&value);
        assert!(json.contains("\"key\":\"value\""));
    }

    #[test]
    fn test_binary_serialize_i32() {
        let bytes = BinarySerializer::serialize_i32(42);
        assert_eq!(bytes.len(), 4);
        assert_eq!(BinarySerializer::deserialize_i32(&bytes), Some(42));
    }

    #[test]
    fn test_binary_serialize_i64() {
        let bytes = BinarySerializer::serialize_i64(12345678901234);
        assert_eq!(bytes.len(), 8);
        assert_eq!(
            BinarySerializer::deserialize_i64(&bytes),
            Some(12345678901234)
        );
    }

    #[test]
    fn test_binary_serialize_f32() {
        let bytes = BinarySerializer::serialize_f32(42.5);
        assert_eq!(bytes.len(), 4);
        let result = BinarySerializer::deserialize_f32(&bytes).unwrap();
        assert!((result - 42.5).abs() < 0.01);
    }

    #[test]
    fn test_binary_serialize_f64() {
        let bytes = BinarySerializer::serialize_f64(123.456789);
        assert_eq!(bytes.len(), 8);
        assert_eq!(BinarySerializer::deserialize_f64(&bytes), Some(123.456789));
    }

    #[test]
    fn test_binary_deserialize_invalid() {
        assert_eq!(BinarySerializer::deserialize_i32(&[1, 2]), None);
        assert_eq!(BinarySerializer::deserialize_i64(&[1, 2, 3]), None);
    }
}
