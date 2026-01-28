#[cfg(windows)]
use std::ffi::OsStr;
#[cfg(windows)]
use std::os::windows::ffi::OsStrExt;
#[cfg(windows)]
use std::ptr;

#[cfg(windows)]
#[link(name = "user32")]
extern "system" {
    fn MessageBoxW(
        hwnd: *mut std::ffi::c_void,
        text: *const u16,
        caption: *const u16,
        utype: u32,
    ) -> i32;
}

#[cfg(windows)]
#[link(name = "kernel32")]
extern "system" {
    fn GetComputerNameW(buffer: *mut u16, size: *mut u32) -> i32;
    fn GetSystemDirectoryW(buffer: *mut u16, size: u32) -> u32;
    fn GetWindowsDirectoryW(buffer: *mut u16, size: u32) -> u32;
    fn GetTempPathW(buffer_length: u32, buffer: *mut u16) -> u32;
    fn GetCurrentDirectoryW(buffer_length: u32, buffer: *mut u16) -> u32;
    fn SetCurrentDirectoryW(path: *const u16) -> i32;
}

#[cfg(windows)]
fn to_wide_string(s: &str) -> Vec<u16> {
    OsStr::new(s).encode_wide().chain(Some(0)).collect()
}

#[cfg(windows)]
fn from_wide_string(wide: &[u16]) -> String {
    let len = wide.iter().position(|&c| c == 0).unwrap_or(wide.len());
    String::from_utf16_lossy(&wide[..len])
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MessageBoxType {
    Ok,
    OkCancel,
    YesNo,
    YesNoCancel,
}

#[cfg(windows)]
impl MessageBoxType {
    fn to_flags(self) -> u32 {
        match self {
            MessageBoxType::Ok => 0x00000000,
            MessageBoxType::OkCancel => 0x00000001,
            MessageBoxType::YesNo => 0x00000004,
            MessageBoxType::YesNoCancel => 0x00000003,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MessageBoxResult {
    Ok,
    Cancel,
    Yes,
    No,
}

pub struct Win32;

impl Win32 {
    #[cfg(windows)]
    pub fn message_box(
        title: &str,
        message: &str,
        box_type: MessageBoxType,
    ) -> Result<MessageBoxResult, String> {
        let title_wide = to_wide_string(title);
        let message_wide = to_wide_string(message);

        let result = unsafe {
            MessageBoxW(
                ptr::null_mut(),
                message_wide.as_ptr(),
                title_wide.as_ptr(),
                box_type.to_flags(),
            )
        };

        match result {
            1 => Ok(MessageBoxResult::Ok),
            2 => Ok(MessageBoxResult::Cancel),
            6 => Ok(MessageBoxResult::Yes),
            7 => Ok(MessageBoxResult::No),
            _ => Err(format!("Unknown message box result: {result}")),
        }
    }

    #[cfg(not(windows))]
    pub fn message_box(
        _title: &str,
        _message: &str,
        _box_type: MessageBoxType,
    ) -> Result<MessageBoxResult, String> {
        Err("Win32 API is only available on Windows".to_string())
    }

    #[cfg(windows)]
    pub fn get_computer_name() -> Result<String, String> {
        let mut buffer = vec![0u16; 256];
        let mut size = buffer.len() as u32;

        let result = unsafe { GetComputerNameW(buffer.as_mut_ptr(), &mut size) };

        if result != 0 {
            Ok(from_wide_string(&buffer))
        } else {
            Err("Failed to get computer name".to_string())
        }
    }

    #[cfg(not(windows))]
    pub fn get_computer_name() -> Result<String, String> {
        Err("Win32 API is only available on Windows".to_string())
    }

    #[cfg(windows)]
    pub fn get_system_directory() -> Result<String, String> {
        let mut buffer = vec![0u16; 260];

        let result = unsafe { GetSystemDirectoryW(buffer.as_mut_ptr(), buffer.len() as u32) };

        if result > 0 {
            Ok(from_wide_string(&buffer))
        } else {
            Err("Failed to get system directory".to_string())
        }
    }

    #[cfg(not(windows))]
    pub fn get_system_directory() -> Result<String, String> {
        Err("Win32 API is only available on Windows".to_string())
    }

    #[cfg(windows)]
    pub fn get_windows_directory() -> Result<String, String> {
        let mut buffer = vec![0u16; 260];

        let result = unsafe { GetWindowsDirectoryW(buffer.as_mut_ptr(), buffer.len() as u32) };

        if result > 0 {
            Ok(from_wide_string(&buffer))
        } else {
            Err("Failed to get Windows directory".to_string())
        }
    }

    #[cfg(not(windows))]
    pub fn get_windows_directory() -> Result<String, String> {
        Err("Win32 API is only available on Windows".to_string())
    }

    #[cfg(windows)]
    pub fn get_temp_path() -> Result<String, String> {
        let mut buffer = vec![0u16; 260];

        let result = unsafe { GetTempPathW(buffer.len() as u32, buffer.as_mut_ptr()) };

        if result > 0 {
            Ok(from_wide_string(&buffer))
        } else {
            Err("Failed to get temp path".to_string())
        }
    }

    #[cfg(not(windows))]
    pub fn get_temp_path() -> Result<String, String> {
        Err("Win32 API is only available on Windows".to_string())
    }

    #[cfg(windows)]
    pub fn get_current_directory() -> Result<String, String> {
        let mut buffer = vec![0u16; 260];

        let result = unsafe { GetCurrentDirectoryW(buffer.len() as u32, buffer.as_mut_ptr()) };

        if result > 0 {
            Ok(from_wide_string(&buffer))
        } else {
            Err("Failed to get current directory".to_string())
        }
    }

    #[cfg(not(windows))]
    pub fn get_current_directory() -> Result<String, String> {
        Err("Win32 API is only available on Windows".to_string())
    }

    #[cfg(windows)]
    pub fn set_current_directory(path: &str) -> Result<(), String> {
        let path_wide = to_wide_string(path);

        let result = unsafe { SetCurrentDirectoryW(path_wide.as_ptr()) };

        if result != 0 {
            Ok(())
        } else {
            Err("Failed to set current directory".to_string())
        }
    }

    #[cfg(not(windows))]
    pub fn set_current_directory(_path: &str) -> Result<(), String> {
        Err("Win32 API is only available on Windows".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(windows)]
    fn test_get_computer_name() {
        let result = Win32::get_computer_name();
        assert!(result.is_ok());
        let name = result.unwrap();
        assert!(!name.is_empty());
    }

    #[test]
    #[cfg(not(windows))]
    fn test_win32_not_available() {
        let result = Win32::get_computer_name();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("only available on Windows"));
    }

    #[test]
    #[cfg(windows)]
    fn test_get_system_directory() {
        let result = Win32::get_system_directory();
        assert!(result.is_ok());
        let dir = result.unwrap();
        assert!(!dir.is_empty());
        assert!(dir.to_lowercase().contains("system"));
    }

    #[test]
    #[cfg(windows)]
    fn test_get_windows_directory() {
        let result = Win32::get_windows_directory();
        assert!(result.is_ok());
        let dir = result.unwrap();
        assert!(!dir.is_empty());
    }

    #[test]
    #[cfg(windows)]
    fn test_get_temp_path() {
        let result = Win32::get_temp_path();
        assert!(result.is_ok());
        let path = result.unwrap();
        assert!(!path.is_empty());
    }

    #[test]
    #[cfg(windows)]
    fn test_get_current_directory() {
        let result = Win32::get_current_directory();
        assert!(result.is_ok());
        let dir = result.unwrap();
        assert!(!dir.is_empty());
    }

    #[test]
    fn test_message_box_types() {
        let _type1 = MessageBoxType::Ok;
        let _type2 = MessageBoxType::OkCancel;
        let _type3 = MessageBoxType::YesNo;
        let _type4 = MessageBoxType::YesNoCancel;
    }
}
