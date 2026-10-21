//! Binary (`!!binary`) scalars and their base64 encoding.

use base64::{engine::general_purpose, Engine as _};

use super::{ScalarStyle, ScalarType, ScalarValue};

/// Base64 encode bytes for binary data
fn base64_encode(input: &[u8]) -> String {
    general_purpose::STANDARD.encode(input)
}

/// Base64 decode string back to bytes
pub(super) fn base64_decode(input: &str) -> Result<Vec<u8>, String> {
    general_purpose::STANDARD
        .decode(input.trim())
        .map_err(|e| format!("Base64 decode error: {e}"))
}

impl ScalarValue {
    /// Create a binary scalar from raw bytes
    pub fn binary(data: &[u8]) -> Self {
        let encoded = base64_encode(data);
        Self {
            value: encoded,
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Binary,
            tag: None,
        }
    }

    /// Extract binary data if this is a binary scalar
    pub fn as_binary(&self) -> Option<Result<Vec<u8>, String>> {
        match self.scalar_type {
            ScalarType::Binary => Some(base64_decode(&self.value)),
            _ => None,
        }
    }

    /// Check if this is a binary scalar
    pub fn is_binary(&self) -> bool {
        self.scalar_type == ScalarType::Binary
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_data_encoding() {
        // Test creating binary scalar from raw bytes
        let data = b"Hello, World!";
        let scalar = ScalarValue::binary(data);

        assert!(scalar.is_binary());
        assert_eq!(scalar.scalar_type(), ScalarType::Binary);

        // Should produce valid base64
        let yaml_output = scalar.to_yaml_string();
        assert!(yaml_output.starts_with("!!binary "));

        // Should be able to decode back to original data
        if let Some(decoded_result) = scalar.as_binary() {
            let decoded = decoded_result.expect("Should decode successfully");
            assert_eq!(decoded, data);
        } else {
            panic!("Should be able to extract binary data");
        }
    }

    #[test]
    fn test_base64_encoding_decoding() {
        // Test various byte sequences
        let test_cases = [
            b"".as_slice(),
            b"A",
            b"AB",
            b"ABC",
            b"ABCD",
            b"Hello, World!",
            &[0, 1, 2, 3, 255, 254, 253],
        ];

        for data in test_cases {
            let encoded = base64_encode(data);
            let decoded = base64_decode(&encoded).expect("Should decode successfully");
            assert_eq!(decoded, data, "Failed for data: {:?}", data);
        }
    }
}
