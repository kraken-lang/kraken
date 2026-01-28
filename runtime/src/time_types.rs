//! Time types: Duration, Instant, SystemTime for time measurements and spans.

#![allow(dead_code)]

use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

/// Duration utilities for time spans
pub struct DurationUtils;

impl DurationUtils {
    /// Create a duration from seconds
    pub fn from_secs(secs: u64) -> Duration {
        Duration::from_secs(secs)
    }

    /// Create a duration from milliseconds
    pub fn from_millis(millis: u64) -> Duration {
        Duration::from_millis(millis)
    }

    /// Create a duration from microseconds
    pub fn from_micros(micros: u64) -> Duration {
        Duration::from_micros(micros)
    }

    /// Create a duration from nanoseconds
    pub fn from_nanos(nanos: u64) -> Duration {
        Duration::from_nanos(nanos)
    }

    /// Get duration as seconds
    pub fn as_secs(duration: &Duration) -> u64 {
        duration.as_secs()
    }

    /// Get duration as milliseconds
    pub fn as_millis(duration: &Duration) -> u128 {
        duration.as_millis()
    }

    /// Get duration as microseconds
    pub fn as_micros(duration: &Duration) -> u128 {
        duration.as_micros()
    }

    /// Get duration as nanoseconds
    pub fn as_nanos(duration: &Duration) -> u128 {
        duration.as_nanos()
    }

    /// Add two durations
    pub fn add(a: Duration, b: Duration) -> Duration {
        a + b
    }

    /// Subtract two durations
    pub fn sub(a: Duration, b: Duration) -> Option<Duration> {
        a.checked_sub(b)
    }

    /// Multiply duration by scalar
    pub fn mul(duration: Duration, scalar: u32) -> Duration {
        duration * scalar
    }

    /// Divide duration by scalar
    pub fn div(duration: Duration, scalar: u32) -> Duration {
        duration / scalar
    }

    /// Check if duration is zero
    pub fn is_zero(duration: &Duration) -> bool {
        duration.is_zero()
    }
}

/// Instant utilities for time measurements
pub struct InstantUtils;

impl InstantUtils {
    /// Get current instant
    pub fn now() -> Instant {
        Instant::now()
    }

    /// Get elapsed time since instant
    pub fn elapsed(instant: &Instant) -> Duration {
        instant.elapsed()
    }

    /// Get duration between two instants
    pub fn duration_since(later: &Instant, earlier: &Instant) -> Duration {
        later.duration_since(*earlier)
    }

    /// Check if instant is in the past
    pub fn is_past(instant: &Instant) -> bool {
        instant.elapsed() > Duration::from_secs(0)
    }
}

/// SystemTime utilities for wall-clock time
pub struct SystemTimeUtils;

impl SystemTimeUtils {
    /// Get current system time
    pub fn now() -> SystemTime {
        SystemTime::now()
    }

    /// Get duration since UNIX epoch
    pub fn since_epoch(time: &SystemTime) -> Result<Duration, std::time::SystemTimeError> {
        time.duration_since(UNIX_EPOCH)
    }

    /// Get elapsed time since system time
    pub fn elapsed(time: &SystemTime) -> Result<Duration, std::time::SystemTimeError> {
        time.elapsed()
    }

    /// Add duration to system time
    pub fn add(time: SystemTime, duration: Duration) -> SystemTime {
        time + duration
    }

    /// Subtract duration from system time
    pub fn sub(time: SystemTime, duration: Duration) -> SystemTime {
        time - duration
    }

    /// Get duration between two system times
    pub fn duration_since(
        later: &SystemTime,
        earlier: &SystemTime,
    ) -> Result<Duration, std::time::SystemTimeError> {
        later.duration_since(*earlier)
    }
}

/// CString utilities for C FFI strings
pub struct CStringUtils;

impl CStringUtils {
    /// Create a CString from a string
    pub fn from_string(s: &str) -> Result<std::ffi::CString, std::ffi::NulError> {
        std::ffi::CString::new(s)
    }

    /// Convert CString to String
    pub fn to_string(c_str: &std::ffi::CStr) -> Result<String, std::str::Utf8Error> {
        c_str.to_str().map(|s| s.to_string())
    }

    /// Convert CString to String with lossy conversion
    pub fn to_string_lossy(c_str: &std::ffi::CStr) -> String {
        c_str.to_string_lossy().to_string()
    }

    /// Get length of CString
    pub fn len(c_str: &std::ffi::CStr) -> usize {
        c_str.to_bytes().len()
    }

    /// Check if CString is empty
    pub fn is_empty(c_str: &std::ffi::CStr) -> bool {
        c_str.to_bytes().is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_duration_from_secs() {
        let duration = DurationUtils::from_secs(5);
        assert_eq!(DurationUtils::as_secs(&duration), 5);
    }

    #[test]
    fn test_duration_from_millis() {
        let duration = DurationUtils::from_millis(1000);
        assert_eq!(DurationUtils::as_millis(&duration), 1000);
    }

    #[test]
    fn test_duration_add() {
        let a = DurationUtils::from_secs(2);
        let b = DurationUtils::from_secs(3);
        let result = DurationUtils::add(a, b);
        assert_eq!(DurationUtils::as_secs(&result), 5);
    }

    #[test]
    fn test_duration_sub() {
        let a = DurationUtils::from_secs(5);
        let b = DurationUtils::from_secs(2);
        let result = DurationUtils::sub(a, b);
        assert_eq!(result.map(|d| DurationUtils::as_secs(&d)), Some(3));
    }

    #[test]
    fn test_duration_mul() {
        let duration = DurationUtils::from_secs(2);
        let result = DurationUtils::mul(duration, 3);
        assert_eq!(DurationUtils::as_secs(&result), 6);
    }

    #[test]
    fn test_duration_div() {
        let duration = DurationUtils::from_secs(6);
        let result = DurationUtils::div(duration, 3);
        assert_eq!(DurationUtils::as_secs(&result), 2);
    }

    #[test]
    fn test_duration_is_zero() {
        let zero = Duration::from_secs(0);
        assert!(DurationUtils::is_zero(&zero));

        let non_zero = Duration::from_secs(1);
        assert!(!DurationUtils::is_zero(&non_zero));
    }

    #[test]
    fn test_instant_now() {
        let instant = InstantUtils::now();
        let elapsed = InstantUtils::elapsed(&instant);
        // Elapsed time is always non-negative by definition
        assert!(elapsed >= Duration::from_secs(0));
    }

    #[test]
    fn test_instant_duration_since() {
        let earlier = InstantUtils::now();
        std::thread::sleep(Duration::from_millis(10));
        let later = InstantUtils::now();

        let duration = InstantUtils::duration_since(&later, &earlier);
        assert!(DurationUtils::as_millis(&duration) >= 10);
    }

    #[test]
    fn test_system_time_now() {
        let time = SystemTimeUtils::now();
        let since_epoch = SystemTimeUtils::since_epoch(&time);
        assert!(since_epoch.is_ok());
    }

    #[test]
    fn test_system_time_add() {
        let time = SystemTimeUtils::now();
        let duration = DurationUtils::from_secs(10);
        let later = SystemTimeUtils::add(time, duration);

        let diff = SystemTimeUtils::duration_since(&later, &time);
        assert_eq!(diff.ok().map(|d| DurationUtils::as_secs(&d)), Some(10));
    }

    #[test]
    fn test_system_time_sub() {
        let time = SystemTimeUtils::now();
        let duration = DurationUtils::from_secs(10);
        let earlier = SystemTimeUtils::sub(time, duration);

        let diff = SystemTimeUtils::duration_since(&time, &earlier);
        assert_eq!(diff.ok().map(|d| DurationUtils::as_secs(&d)), Some(10));
    }

    #[test]
    fn test_cstring_from_string() {
        let c_str = CStringUtils::from_string("hello");
        assert!(c_str.is_ok());
    }

    #[test]
    fn test_cstring_to_string() {
        let c_str = CString::new("hello").unwrap();
        let result = CStringUtils::to_string(&c_str);
        assert_eq!(result.ok(), Some("hello".to_string()));
    }

    #[test]
    fn test_cstring_to_string_lossy() {
        let c_str = CString::new("hello").unwrap();
        let result = CStringUtils::to_string_lossy(&c_str);
        assert_eq!(result, "hello");
    }

    #[test]
    fn test_cstring_len() {
        let c_str = CString::new("hello").unwrap();
        assert_eq!(CStringUtils::len(&c_str), 5);
    }

    #[test]
    fn test_cstring_is_empty() {
        let empty = CString::new("").unwrap();
        assert!(CStringUtils::is_empty(&empty));

        let non_empty = CString::new("hello").unwrap();
        assert!(!CStringUtils::is_empty(&non_empty));
    }
}
