use std::fmt;
use std::ops::{Add, Sub};
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Date {
    year: i32,
    month: u8,
    day: u8,
}

impl Date {
    pub fn new(year: i32, month: u8, day: u8) -> Result<Self, String> {
        if !(1..=12).contains(&month) {
            return Err(format!("Invalid month: {month}"));
        }
        let max_day = Self::days_in_month(year, month);
        if !(1..=max_day).contains(&day) {
            return Err(format!("Invalid day: {day} for month {month}"));
        }
        Ok(Date { year, month, day })
    }

    pub fn today() -> Self {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        Self::from_unix_timestamp(now as i64)
    }

    pub fn from_unix_timestamp(timestamp: i64) -> Self {
        let days = timestamp / 86400;
        let (year, month, day) = Self::days_to_ymd(days);
        Date { year, month, day }
    }

    pub fn to_unix_timestamp(&self) -> i64 {
        Self::ymd_to_days(self.year, self.month, self.day) * 86400
    }

    pub fn year(&self) -> i32 {
        self.year
    }

    pub fn month(&self) -> u8 {
        self.month
    }

    pub fn day(&self) -> u8 {
        self.day
    }

    pub fn is_leap_year(year: i32) -> bool {
        (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
    }

    fn days_in_month(year: i32, month: u8) -> u8 {
        match month {
            1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
            4 | 6 | 9 | 11 => 30,
            2 => {
                if Self::is_leap_year(year) {
                    29
                } else {
                    28
                }
            }
            _ => 0,
        }
    }

    fn ymd_to_days(year: i32, month: u8, day: u8) -> i64 {
        let mut y = year as i64;
        let m = month as i64;
        let d = day as i64;

        if m <= 2 {
            y -= 1;
        }

        let era = if y >= 0 { y } else { y - 399 } / 400;
        let yoe = y - era * 400;
        let doy = (153 * (m + if m > 2 { -3 } else { 9 }) + 2) / 5 + d - 1;
        let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;

        era * 146097 + doe - 719468
    }

    fn days_to_ymd(days: i64) -> (i32, u8, u8) {
        let z = days + 719468;
        let era = if z >= 0 { z } else { z - 146096 } / 146097;
        let doe = z - era * 146097;
        let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
        let y = yoe + era * 400;
        let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        let mp = (5 * doy + 2) / 153;
        let d = doy - (153 * mp + 2) / 5 + 1;
        let m = mp + if mp < 10 { 3 } else { -9 };
        let year = (y + if m <= 2 { 1 } else { 0 }) as i32;

        (year, m as u8, d as u8)
    }

    pub fn format_iso8601(&self) -> String {
        format!("{:04}-{:02}-{:02}", self.year, self.month, self.day)
    }

    pub fn parse_iso8601(s: &str) -> Result<Self, String> {
        let parts: Vec<&str> = s.split('-').collect();
        if parts.len() != 3 {
            return Err("Invalid ISO 8601 date format".to_string());
        }
        let year = parts[0]
            .parse::<i32>()
            .map_err(|_| "Invalid year".to_string())?;
        let month = parts[1]
            .parse::<u8>()
            .map_err(|_| "Invalid month".to_string())?;
        let day = parts[2]
            .parse::<u8>()
            .map_err(|_| "Invalid day".to_string())?;
        Self::new(year, month, day)
    }
}

impl fmt::Display for Date {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_iso8601())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time {
    hour: u8,
    minute: u8,
    second: u8,
    nanosecond: u32,
}

impl Time {
    pub fn new(hour: u8, minute: u8, second: u8, nanosecond: u32) -> Result<Self, String> {
        if hour >= 24 {
            return Err(format!("Invalid hour: {hour}"));
        }
        if minute >= 60 {
            return Err(format!("Invalid minute: {minute}"));
        }
        if second >= 60 {
            return Err(format!("Invalid second: {second}"));
        }
        if nanosecond >= 1_000_000_000 {
            return Err(format!("Invalid nanosecond: {nanosecond}"));
        }
        Ok(Time {
            hour,
            minute,
            second,
            nanosecond,
        })
    }

    pub fn now() -> Self {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap();
        let secs = now.as_secs();
        let nanos = now.subsec_nanos();
        let total_secs = secs % 86400;
        let hour = (total_secs / 3600) as u8;
        let minute = ((total_secs % 3600) / 60) as u8;
        let second = (total_secs % 60) as u8;
        Time {
            hour,
            minute,
            second,
            nanosecond: nanos,
        }
    }

    pub fn hour(&self) -> u8 {
        self.hour
    }

    pub fn minute(&self) -> u8 {
        self.minute
    }

    pub fn second(&self) -> u8 {
        self.second
    }

    pub fn nanosecond(&self) -> u32 {
        self.nanosecond
    }

    pub fn format_iso8601(&self) -> String {
        if self.nanosecond == 0 {
            format!("{:02}:{:02}:{:02}", self.hour, self.minute, self.second)
        } else {
            format!(
                "{:02}:{:02}:{:02}.{:09}",
                self.hour, self.minute, self.second, self.nanosecond
            )
        }
    }

    pub fn parse_iso8601(s: &str) -> Result<Self, String> {
        let parts: Vec<&str> = s.split(':').collect();
        if parts.len() < 2 {
            return Err("Invalid ISO 8601 time format".to_string());
        }
        let hour = parts[0]
            .parse::<u8>()
            .map_err(|_| "Invalid hour".to_string())?;
        let minute = parts[1]
            .parse::<u8>()
            .map_err(|_| "Invalid minute".to_string())?;

        let (second, nanosecond) = if parts.len() >= 3 {
            let sec_parts: Vec<&str> = parts[2].split('.').collect();
            let second = sec_parts[0]
                .parse::<u8>()
                .map_err(|_| "Invalid second".to_string())?;
            let nanosecond = if sec_parts.len() > 1 {
                let nanos_str = format!("{:0<9}", sec_parts[1]);
                nanos_str
                    .parse::<u32>()
                    .map_err(|_| "Invalid nanosecond".to_string())?
            } else {
                0
            };
            (second, nanosecond)
        } else {
            (0, 0)
        };

        Self::new(hour, minute, second, nanosecond)
    }
}

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_iso8601())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct DateTime {
    date: Date,
    time: Time,
    offset_seconds: i32,
}

impl DateTime {
    pub fn new(date: Date, time: Time, offset_seconds: i32) -> Self {
        DateTime {
            date,
            time,
            offset_seconds,
        }
    }

    pub fn now() -> Self {
        DateTime {
            date: Date::today(),
            time: Time::now(),
            offset_seconds: 0,
        }
    }

    pub fn now_utc() -> Self {
        Self::now()
    }

    pub fn from_unix_timestamp(timestamp: i64) -> Self {
        let date = Date::from_unix_timestamp(timestamp);
        let secs = (timestamp % 86400) as u32;
        let hour = (secs / 3600) as u8;
        let minute = ((secs % 3600) / 60) as u8;
        let second = (secs % 60) as u8;
        let time = Time::new(hour, minute, second, 0).unwrap();
        DateTime {
            date,
            time,
            offset_seconds: 0,
        }
    }

    pub fn to_unix_timestamp(&self) -> i64 {
        let date_secs = self.date.to_unix_timestamp();
        let time_secs = self.time.hour() as i64 * 3600
            + self.time.minute() as i64 * 60
            + self.time.second() as i64;
        date_secs + time_secs - self.offset_seconds as i64
    }

    pub fn date(&self) -> Date {
        self.date
    }

    pub fn time(&self) -> Time {
        self.time
    }

    pub fn offset_seconds(&self) -> i32 {
        self.offset_seconds
    }

    pub fn with_offset(mut self, offset_seconds: i32) -> Self {
        self.offset_seconds = offset_seconds;
        self
    }

    pub fn format_iso8601(&self) -> String {
        if self.offset_seconds == 0 {
            format!("{}T{}Z", self.date.format_iso8601(), self.time.format_iso8601())
        } else {
            let offset_hours = self.offset_seconds / 3600;
            let offset_mins = (self.offset_seconds.abs() % 3600) / 60;
            let sign = if self.offset_seconds >= 0 { '+' } else { '-' };
            format!(
                "{}T{}{}{:02}:{:02}",
                self.date.format_iso8601(),
                self.time.format_iso8601(),
                sign,
                offset_hours.abs(),
                offset_mins
            )
        }
    }

    pub fn format_rfc3339(&self) -> String {
        self.format_iso8601()
    }

    pub fn parse_iso8601(s: &str) -> Result<Self, String> {
        let parts: Vec<&str> = s.split('T').collect();
        if parts.len() != 2 {
            return Err("Invalid ISO 8601 datetime format".to_string());
        }

        let date = Date::parse_iso8601(parts[0])?;

        let time_part = parts[1];
        let (time_str, offset_seconds) = if let Some(stripped) = time_part.strip_suffix('Z') {
            (stripped, 0)
        } else if time_part.contains('+') || time_part.matches('-').count() > 2 {
            let split_pos = time_part.rfind(&['+', '-'][..]).unwrap();
            let time_str = &time_part[..split_pos];
            let offset_str = &time_part[split_pos..];
            let offset = Self::parse_offset(offset_str)?;
            (time_str, offset)
        } else {
            (time_part, 0)
        };

        let time = Time::parse_iso8601(time_str)?;

        Ok(DateTime {
            date,
            time,
            offset_seconds,
        })
    }

    fn parse_offset(s: &str) -> Result<i32, String> {
        let sign = if s.starts_with('+') { 1 } else { -1 };
        let parts: Vec<&str> = s[1..].split(':').collect();
        if parts.is_empty() {
            return Err("Invalid offset format".to_string());
        }
        let hours = parts[0]
            .parse::<i32>()
            .map_err(|_| "Invalid offset hours".to_string())?;
        let minutes = if parts.len() > 1 {
            parts[1]
                .parse::<i32>()
                .map_err(|_| "Invalid offset minutes".to_string())?
        } else {
            0
        };
        Ok(sign * (hours * 3600 + minutes * 60))
    }
}

impl fmt::Display for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_iso8601())
    }
}

impl Add<std::time::Duration> for DateTime {
    type Output = DateTime;

    fn add(self, duration: std::time::Duration) -> Self::Output {
        let timestamp = self.to_unix_timestamp() + duration.as_secs() as i64;
        DateTime::from_unix_timestamp(timestamp).with_offset(self.offset_seconds)
    }
}

impl Sub<std::time::Duration> for DateTime {
    type Output = DateTime;

    fn sub(self, duration: std::time::Duration) -> Self::Output {
        let timestamp = self.to_unix_timestamp() - duration.as_secs() as i64;
        DateTime::from_unix_timestamp(timestamp).with_offset(self.offset_seconds)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UtcOffset {
    seconds: i32,
}

impl UtcOffset {
    pub fn new(hours: i8, minutes: i8) -> Result<Self, String> {
        if !(-23..=23).contains(&hours) {
            return Err(format!("Invalid offset hours: {hours}"));
        }
        if !(-59..=59).contains(&minutes) {
            return Err(format!("Invalid offset minutes: {minutes}"));
        }
        let seconds = hours as i32 * 3600 + minutes as i32 * 60;
        Ok(UtcOffset { seconds })
    }

    pub fn utc() -> Self {
        UtcOffset { seconds: 0 }
    }

    pub fn seconds(&self) -> i32 {
        self.seconds
    }

    pub fn hours(&self) -> i8 {
        (self.seconds / 3600) as i8
    }

    pub fn minutes(&self) -> i8 {
        ((self.seconds % 3600) / 60) as i8
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_date_new() {
        let date = Date::new(2026, 1, 28).unwrap();
        assert_eq!(date.year(), 2026);
        assert_eq!(date.month(), 1);
        assert_eq!(date.day(), 28);
    }

    #[test]
    fn test_date_invalid() {
        assert!(Date::new(2026, 13, 1).is_err());
        assert!(Date::new(2026, 2, 30).is_err());
        assert!(Date::new(2026, 4, 31).is_err());
    }

    #[test]
    fn test_date_leap_year() {
        assert!(Date::is_leap_year(2024));
        assert!(!Date::is_leap_year(2023));
        assert!(Date::is_leap_year(2000));
        assert!(!Date::is_leap_year(1900));
    }

    #[test]
    fn test_date_format_iso8601() {
        let date = Date::new(2026, 1, 28).unwrap();
        assert_eq!(date.format_iso8601(), "2026-01-28");
    }

    #[test]
    fn test_date_parse_iso8601() {
        let date = Date::parse_iso8601("2026-01-28").unwrap();
        assert_eq!(date.year(), 2026);
        assert_eq!(date.month(), 1);
        assert_eq!(date.day(), 28);
    }

    #[test]
    fn test_time_new() {
        let time = Time::new(14, 30, 45, 123456789).unwrap();
        assert_eq!(time.hour(), 14);
        assert_eq!(time.minute(), 30);
        assert_eq!(time.second(), 45);
        assert_eq!(time.nanosecond(), 123456789);
    }

    #[test]
    fn test_time_invalid() {
        assert!(Time::new(24, 0, 0, 0).is_err());
        assert!(Time::new(0, 60, 0, 0).is_err());
        assert!(Time::new(0, 0, 60, 0).is_err());
        assert!(Time::new(0, 0, 0, 1_000_000_000).is_err());
    }

    #[test]
    fn test_time_format_iso8601() {
        let time = Time::new(14, 30, 45, 0).unwrap();
        assert_eq!(time.format_iso8601(), "14:30:45");

        let time_with_nanos = Time::new(14, 30, 45, 123456789).unwrap();
        assert_eq!(time_with_nanos.format_iso8601(), "14:30:45.123456789");
    }

    #[test]
    fn test_time_parse_iso8601() {
        let time = Time::parse_iso8601("14:30:45").unwrap();
        assert_eq!(time.hour(), 14);
        assert_eq!(time.minute(), 30);
        assert_eq!(time.second(), 45);

        let time_with_nanos = Time::parse_iso8601("14:30:45.123456789").unwrap();
        assert_eq!(time_with_nanos.nanosecond(), 123456789);
    }

    #[test]
    fn test_datetime_new() {
        let date = Date::new(2026, 1, 28).unwrap();
        let time = Time::new(14, 30, 45, 0).unwrap();
        let dt = DateTime::new(date, time, 0);
        assert_eq!(dt.date(), date);
        assert_eq!(dt.time(), time);
        assert_eq!(dt.offset_seconds(), 0);
    }

    #[test]
    fn test_datetime_format_iso8601() {
        let date = Date::new(2026, 1, 28).unwrap();
        let time = Time::new(14, 30, 45, 0).unwrap();
        let dt = DateTime::new(date, time, 0);
        assert_eq!(dt.format_iso8601(), "2026-01-28T14:30:45Z");

        let dt_with_offset = dt.with_offset(3600);
        assert_eq!(dt_with_offset.format_iso8601(), "2026-01-28T14:30:45+01:00");
    }

    #[test]
    fn test_datetime_parse_iso8601() {
        let dt = DateTime::parse_iso8601("2026-01-28T14:30:45Z").unwrap();
        assert_eq!(dt.date().year(), 2026);
        assert_eq!(dt.time().hour(), 14);
        assert_eq!(dt.offset_seconds(), 0);

        let dt_with_offset = DateTime::parse_iso8601("2026-01-28T14:30:45+01:00").unwrap();
        assert_eq!(dt_with_offset.offset_seconds(), 3600);
    }

    #[test]
    fn test_datetime_unix_timestamp() {
        let dt = DateTime::from_unix_timestamp(1706450445);
        let timestamp = dt.to_unix_timestamp();
        assert_eq!(timestamp, 1706450445);
    }

    #[test]
    fn test_utc_offset() {
        let offset = UtcOffset::new(5, 30).unwrap();
        assert_eq!(offset.hours(), 5);
        assert_eq!(offset.minutes(), 30);
        assert_eq!(offset.seconds(), 19800);

        let utc = UtcOffset::utc();
        assert_eq!(utc.seconds(), 0);
    }
}
