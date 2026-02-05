//! Temporary file and directory creation utilities.

#![allow(dead_code)]

use std::env;
use std::fs::{self, File};
use std::io;
use std::path::{Path, PathBuf};

/// Temporary file wrapper that auto-deletes on drop
pub struct TempFile {
    path: PathBuf,
    keep: bool,
}

impl TempFile {
    /// Create a new temporary file with a random name
    pub fn new() -> io::Result<Self> {
        Self::new_in(env::temp_dir())
    }

    /// Create a new temporary file in a specific directory
    pub fn new_in<P: AsRef<Path>>(dir: P) -> io::Result<Self> {
        let path = Self::generate_temp_path(dir.as_ref(), "tmp", "")?;
        File::create(&path)?;
        Ok(Self { path, keep: false })
    }

    /// Create a new temporary file with a specific prefix
    pub fn new_with_prefix<P: AsRef<Path>>(dir: P, prefix: &str) -> io::Result<Self> {
        let path = Self::generate_temp_path(dir.as_ref(), prefix, "")?;
        File::create(&path)?;
        Ok(Self { path, keep: false })
    }

    /// Create a new temporary file with a specific prefix and suffix
    pub fn new_with_prefix_suffix<P: AsRef<Path>>(
        dir: P,
        prefix: &str,
        suffix: &str,
    ) -> io::Result<Self> {
        let path = Self::generate_temp_path(dir.as_ref(), prefix, suffix)?;
        File::create(&path)?;
        Ok(Self { path, keep: false })
    }

    /// Get the path to the temporary file
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Keep the temporary file (don't delete on drop)
    pub fn keep(&mut self) {
        self.keep = true;
    }

    /// Persist the temporary file to a new location
    pub fn persist<P: AsRef<Path>>(mut self, new_path: P) -> io::Result<()> {
        fs::rename(&self.path, new_path)?;
        self.keep = true;
        Ok(())
    }

    /// Close and delete the temporary file immediately
    pub fn close(mut self) -> io::Result<()> {
        self.keep = false;
        drop(self);
        Ok(())
    }

    fn generate_temp_path(dir: &Path, prefix: &str, suffix: &str) -> io::Result<PathBuf> {
        use std::sync::atomic::{AtomicU64, Ordering};
        static COUNTER: AtomicU64 = AtomicU64::new(0);

        for _ in 0..100 {
            let count = COUNTER.fetch_add(1, Ordering::Relaxed);
            let random = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64;
            
            let filename = format!("{prefix}{random:x}{count:x}{suffix}");
            let path = dir.join(filename);

            if !path.exists() {
                return Ok(path);
            }
        }

        Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            "Failed to generate unique temporary file name",
        ))
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        if !self.keep && self.path.exists() {
            let _ = fs::remove_file(&self.path);
        }
    }
}

impl Default for TempFile {
    fn default() -> Self {
        Self::new().expect("Failed to create temporary file")
    }
}

/// Temporary directory wrapper that auto-deletes on drop
pub struct TempDir {
    path: PathBuf,
    keep: bool,
}

impl TempDir {
    /// Create a new temporary directory with a random name
    pub fn new() -> io::Result<Self> {
        Self::new_in(env::temp_dir())
    }

    /// Create a new temporary directory in a specific directory
    pub fn new_in<P: AsRef<Path>>(dir: P) -> io::Result<Self> {
        let path = Self::generate_temp_path(dir.as_ref(), "tmp")?;
        fs::create_dir(&path)?;
        Ok(Self { path, keep: false })
    }

    /// Create a new temporary directory with a specific prefix
    pub fn new_with_prefix<P: AsRef<Path>>(dir: P, prefix: &str) -> io::Result<Self> {
        let path = Self::generate_temp_path(dir.as_ref(), prefix)?;
        fs::create_dir(&path)?;
        Ok(Self { path, keep: false })
    }

    /// Get the path to the temporary directory
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Keep the temporary directory (don't delete on drop)
    pub fn keep(&mut self) {
        self.keep = true;
    }

    /// Persist the temporary directory to a new location
    pub fn persist<P: AsRef<Path>>(mut self, new_path: P) -> io::Result<()> {
        fs::rename(&self.path, new_path)?;
        self.keep = true;
        Ok(())
    }

    /// Close and delete the temporary directory immediately
    pub fn close(mut self) -> io::Result<()> {
        self.keep = false;
        drop(self);
        Ok(())
    }

    /// Create a file in the temporary directory
    pub fn create_file(&self, name: &str) -> io::Result<PathBuf> {
        let path = self.path.join(name);
        File::create(&path)?;
        Ok(path)
    }

    /// Create a subdirectory in the temporary directory
    pub fn create_dir(&self, name: &str) -> io::Result<PathBuf> {
        let path = self.path.join(name);
        fs::create_dir(&path)?;
        Ok(path)
    }

    fn generate_temp_path(dir: &Path, prefix: &str) -> io::Result<PathBuf> {
        use std::sync::atomic::{AtomicU64, Ordering};
        static COUNTER: AtomicU64 = AtomicU64::new(0);

        for _ in 0..100 {
            let count = COUNTER.fetch_add(1, Ordering::Relaxed);
            let random = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64;
            
            let dirname = format!("{prefix}{random:x}{count:x}");
            let path = dir.join(dirname);

            if !path.exists() {
                return Ok(path);
            }
        }

        Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            "Failed to generate unique temporary directory name",
        ))
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        if !self.keep && self.path.exists() {
            let _ = fs::remove_dir_all(&self.path);
        }
    }
}

impl Default for TempDir {
    fn default() -> Self {
        Self::new().expect("Failed to create temporary directory")
    }
}

/// Temporary file utilities
pub struct TempFileUtils;

impl TempFileUtils {
    /// Get the system temporary directory
    pub fn temp_dir() -> PathBuf {
        env::temp_dir()
    }

    /// Create a temporary file and return its path
    pub fn create_temp_file() -> io::Result<PathBuf> {
        let mut temp = TempFile::new()?;
        let path = temp.path().to_path_buf();
        temp.keep();
        Ok(path)
    }

    /// Create a temporary directory and return its path
    pub fn create_temp_dir() -> io::Result<PathBuf> {
        let mut temp = TempDir::new()?;
        let path = temp.path().to_path_buf();
        temp.keep();
        Ok(path)
    }

    /// Create a temporary file with specific content
    pub fn create_temp_file_with_content(content: &[u8]) -> io::Result<PathBuf> {
        let mut temp = TempFile::new()?;
        fs::write(temp.path(), content)?;
        let path = temp.path().to_path_buf();
        temp.keep();
        Ok(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_temp_file_creation() {
        let temp = TempFile::new().unwrap();
        assert!(temp.path().exists());
        let path = temp.path().to_path_buf();
        drop(temp);
        assert!(!path.exists());
    }

    #[test]
    fn test_temp_file_keep() {
        let mut temp = TempFile::new().unwrap();
        let path = temp.path().to_path_buf();
        temp.keep();
        drop(temp);
        assert!(path.exists());
        fs::remove_file(path).unwrap();
    }

    #[test]
    fn test_temp_file_with_prefix() {
        let temp = TempFile::new_with_prefix(env::temp_dir(), "test_").unwrap();
        let filename = temp.path().file_name().unwrap().to_string_lossy();
        assert!(filename.starts_with("test_"));
    }

    #[test]
    fn test_temp_file_with_prefix_suffix() {
        let temp = TempFile::new_with_prefix_suffix(env::temp_dir(), "test_", ".txt").unwrap();
        let filename = temp.path().file_name().unwrap().to_string_lossy();
        assert!(filename.starts_with("test_"));
        assert!(filename.ends_with(".txt"));
    }

    #[test]
    fn test_temp_file_persist() {
        let temp = TempFile::new().unwrap();
        let persist_path = env::temp_dir().join("persisted_file.txt");
        temp.persist(&persist_path).unwrap();
        assert!(persist_path.exists());
        fs::remove_file(persist_path).unwrap();
    }

    #[test]
    fn test_temp_dir_creation() {
        let temp = TempDir::new().unwrap();
        assert!(temp.path().exists());
        assert!(temp.path().is_dir());
        let path = temp.path().to_path_buf();
        drop(temp);
        assert!(!path.exists());
    }

    #[test]
    fn test_temp_dir_keep() {
        let mut temp = TempDir::new().unwrap();
        let path = temp.path().to_path_buf();
        temp.keep();
        drop(temp);
        assert!(path.exists());
        fs::remove_dir_all(path).unwrap();
    }

    #[test]
    fn test_temp_dir_with_prefix() {
        let temp = TempDir::new_with_prefix(env::temp_dir(), "test_").unwrap();
        let dirname = temp.path().file_name().unwrap().to_string_lossy();
        assert!(dirname.starts_with("test_"));
    }

    #[test]
    fn test_temp_dir_create_file() {
        let temp = TempDir::new().unwrap();
        let file_path = temp.create_file("test.txt").unwrap();
        assert!(file_path.exists());
        assert!(file_path.is_file());
    }

    #[test]
    fn test_temp_dir_create_dir() {
        let temp = TempDir::new().unwrap();
        let dir_path = temp.create_dir("subdir").unwrap();
        assert!(dir_path.exists());
        assert!(dir_path.is_dir());
    }

    #[test]
    fn test_temp_dir_persist() {
        let temp = TempDir::new().unwrap();
        let persist_path = env::temp_dir().join("persisted_dir");
        temp.persist(&persist_path).unwrap();
        assert!(persist_path.exists());
        fs::remove_dir_all(persist_path).unwrap();
    }

    #[test]
    fn test_temp_file_utils_create() {
        let path = TempFileUtils::create_temp_file().unwrap();
        assert!(path.exists());
        fs::remove_file(path).unwrap();
    }

    #[test]
    fn test_temp_file_utils_create_dir() {
        let path = TempFileUtils::create_temp_dir().unwrap();
        assert!(path.exists());
        assert!(path.is_dir());
        fs::remove_dir_all(path).unwrap();
    }

    #[test]
    fn test_temp_file_utils_with_content() {
        let content = b"Hello, World!";
        let path = TempFileUtils::create_temp_file_with_content(content).unwrap();
        let read_content = fs::read(&path).unwrap();
        assert_eq!(read_content, content);
        fs::remove_file(path).unwrap();
    }

    #[test]
    fn test_temp_dir_auto_cleanup() {
        let path = {
            let temp = TempDir::new().unwrap();
            temp.create_file("test.txt").unwrap();
            temp.path().to_path_buf()
        };
        assert!(!path.exists());
    }
}
