//! Path types for filesystem operations: Path, PathBuf, OsString, OsStr.

#![allow(dead_code)]

use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};

/// Path utilities for working with filesystem paths
pub struct PathUtils;

impl PathUtils {
    /// Create a new PathBuf from a string
    pub fn from_string<S: AsRef<str>>(s: S) -> PathBuf {
        PathBuf::from(s.as_ref())
    }

    /// Join two paths
    pub fn join<P: AsRef<Path>, Q: AsRef<Path>>(base: P, component: Q) -> PathBuf {
        base.as_ref().join(component)
    }

    /// Get the parent directory
    pub fn parent<P: AsRef<Path>>(path: P) -> Option<PathBuf> {
        path.as_ref().parent().map(|p| p.to_path_buf())
    }

    /// Get the file name
    pub fn file_name<P: AsRef<Path>>(path: P) -> Option<OsString> {
        path.as_ref().file_name().map(|s| s.to_os_string())
    }

    /// Get the file stem (name without extension)
    pub fn file_stem<P: AsRef<Path>>(path: P) -> Option<OsString> {
        path.as_ref().file_stem().map(|s| s.to_os_string())
    }

    /// Get the file extension
    pub fn extension<P: AsRef<Path>>(path: P) -> Option<OsString> {
        path.as_ref().extension().map(|s| s.to_os_string())
    }

    /// Check if path is absolute
    pub fn is_absolute<P: AsRef<Path>>(path: P) -> bool {
        path.as_ref().is_absolute()
    }

    /// Check if path is relative
    pub fn is_relative<P: AsRef<Path>>(path: P) -> bool {
        path.as_ref().is_relative()
    }

    /// Convert path to string
    pub fn to_string<P: AsRef<Path>>(path: P) -> Option<String> {
        path.as_ref().to_str().map(|s| s.to_string())
    }

    /// Get all components of a path
    pub fn components<P: AsRef<Path>>(path: P) -> Vec<String> {
        path.as_ref()
            .components()
            .filter_map(|c| c.as_os_str().to_str().map(|s| s.to_string()))
            .collect()
    }

    /// Canonicalize a path (resolve to absolute path)
    pub fn canonicalize<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
        path.as_ref().canonicalize()
    }

    /// Check if path exists
    pub fn exists<P: AsRef<Path>>(path: P) -> bool {
        path.as_ref().exists()
    }

    /// Check if path is a file
    pub fn is_file<P: AsRef<Path>>(path: P) -> bool {
        path.as_ref().is_file()
    }

    /// Check if path is a directory
    pub fn is_dir<P: AsRef<Path>>(path: P) -> bool {
        path.as_ref().is_dir()
    }
}

/// OsString utilities for platform strings
pub struct OsStringUtils;

impl OsStringUtils {
    /// Create a new OsString from a string
    pub fn from_string<S: AsRef<str>>(s: S) -> OsString {
        OsString::from(s.as_ref())
    }

    /// Convert OsString to String if valid UTF-8
    pub fn to_string(os_str: &OsStr) -> Option<String> {
        os_str.to_str().map(|s| s.to_string())
    }

    /// Convert OsString to String with lossy conversion
    pub fn to_string_lossy(os_str: &OsStr) -> String {
        os_str.to_string_lossy().to_string()
    }

    /// Get length in bytes
    pub fn len(os_str: &OsStr) -> usize {
        os_str.len()
    }

    /// Check if empty
    pub fn is_empty(os_str: &OsStr) -> bool {
        os_str.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_from_string() {
        let path = PathUtils::from_string("/tmp/test.txt");
        assert_eq!(
            PathUtils::to_string(&path),
            Some("/tmp/test.txt".to_string())
        );
    }

    #[test]
    fn test_path_join() {
        let path = PathUtils::join("/tmp", "test.txt");
        assert!(PathUtils::to_string(&path).unwrap().contains("test.txt"));
    }

    #[test]
    fn test_path_file_name() {
        let path = PathBuf::from("/tmp/test.txt");
        let name = PathUtils::file_name(&path);
        assert_eq!(name.as_ref().and_then(|n| n.to_str()), Some("test.txt"));
    }

    #[test]
    fn test_path_file_stem() {
        let path = PathBuf::from("/tmp/test.txt");
        let stem = PathUtils::file_stem(&path);
        assert_eq!(stem.as_ref().and_then(|s| s.to_str()), Some("test"));
    }

    #[test]
    fn test_path_extension() {
        let path = PathBuf::from("/tmp/test.txt");
        let ext = PathUtils::extension(&path);
        assert_eq!(ext.as_ref().and_then(|e| e.to_str()), Some("txt"));
    }

    #[test]
    fn test_path_is_absolute() {
        #[cfg(unix)]
        assert!(PathUtils::is_absolute("/tmp/test.txt"));
        #[cfg(windows)]
        assert!(PathUtils::is_absolute("C:\\temp\\test.txt"));
        assert!(!PathUtils::is_absolute("test.txt"));
    }

    #[test]
    fn test_path_is_relative() {
        #[cfg(unix)]
        assert!(!PathUtils::is_relative("/tmp/test.txt"));
        #[cfg(windows)]
        assert!(!PathUtils::is_relative("C:\\temp\\test.txt"));
        assert!(PathUtils::is_relative("test.txt"));
    }

    #[test]
    fn test_path_components() {
        let path = PathBuf::from("/tmp/test/file.txt");
        let components = PathUtils::components(&path);
        assert!(components.contains(&"tmp".to_string()));
        assert!(components.contains(&"test".to_string()));
    }

    #[test]
    fn test_os_string_from_string() {
        let os_str = OsStringUtils::from_string("test");
        assert_eq!(OsStringUtils::to_string(&os_str), Some("test".to_string()));
    }

    #[test]
    fn test_os_string_to_string_lossy() {
        let os_str = OsString::from("test");
        assert_eq!(OsStringUtils::to_string_lossy(&os_str), "test");
    }

    #[test]
    fn test_os_string_len() {
        let os_str = OsString::from("test");
        assert_eq!(OsStringUtils::len(&os_str), 4);
    }

    #[test]
    fn test_os_string_is_empty() {
        let os_str = OsString::from("");
        assert!(OsStringUtils::is_empty(&os_str));

        let os_str = OsString::from("test");
        assert!(!OsStringUtils::is_empty(&os_str));
    }
}
