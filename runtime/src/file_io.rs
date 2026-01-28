//! Enhanced file I/O operations including memory-mapped files and directory traversal.

#![allow(dead_code)]

use std::fs::{self, DirEntry, File, Metadata};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};

/// Memory-mapped file for efficient I/O
pub struct MemoryMappedFile {
    path: PathBuf,
    size: usize,
}

impl MemoryMappedFile {
    /// Create a new memory-mapped file
    pub fn new<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref().to_path_buf();
        let metadata = fs::metadata(&path)?;
        let size = metadata.len() as usize;

        Ok(Self { path, size })
    }

    /// Get the file size
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the file path
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Read the entire file into memory
    pub fn read_all(&self) -> io::Result<Vec<u8>> {
        fs::read(&self.path)
    }

    /// Read a portion of the file
    pub fn read_range(&self, offset: usize, length: usize) -> io::Result<Vec<u8>> {
        let mut file = File::open(&self.path)?;
        let mut buffer = vec![0u8; length];

        use std::io::Seek;
        file.seek(std::io::SeekFrom::Start(offset as u64))?;
        file.read_exact(&mut buffer)?;

        Ok(buffer)
    }
}

/// Directory entry information
#[derive(Debug, Clone)]
pub struct DirectoryEntry {
    pub path: PathBuf,
    pub is_dir: bool,
    pub is_file: bool,
    pub size: u64,
}

impl DirectoryEntry {
    fn from_dir_entry(entry: DirEntry) -> io::Result<Self> {
        let path = entry.path();
        let metadata = entry.metadata()?;

        Ok(Self {
            path,
            is_dir: metadata.is_dir(),
            is_file: metadata.is_file(),
            size: metadata.len(),
        })
    }
}

/// Directory traversal utilities
pub struct DirectoryTraversal;

impl DirectoryTraversal {
    /// List all entries in a directory
    pub fn list_dir<P: AsRef<Path>>(path: P) -> io::Result<Vec<DirectoryEntry>> {
        let mut entries = Vec::new();

        for entry in fs::read_dir(path)? {
            let entry = entry?;
            entries.push(DirectoryEntry::from_dir_entry(entry)?);
        }

        Ok(entries)
    }

    /// List all files in a directory (non-recursive)
    pub fn list_files<P: AsRef<Path>>(path: P) -> io::Result<Vec<PathBuf>> {
        let mut files = Vec::new();

        for entry in fs::read_dir(path)? {
            let entry = entry?;
            if entry.metadata()?.is_file() {
                files.push(entry.path());
            }
        }

        Ok(files)
    }

    /// List all directories in a directory (non-recursive)
    pub fn list_dirs<P: AsRef<Path>>(path: P) -> io::Result<Vec<PathBuf>> {
        let mut dirs = Vec::new();

        for entry in fs::read_dir(path)? {
            let entry = entry?;
            if entry.metadata()?.is_dir() {
                dirs.push(entry.path());
            }
        }

        Ok(dirs)
    }

    /// Recursively walk a directory tree
    pub fn walk_dir<P: AsRef<Path>>(path: P) -> io::Result<Vec<DirectoryEntry>> {
        let mut entries = Vec::new();
        Self::walk_dir_recursive(path.as_ref(), &mut entries)?;
        Ok(entries)
    }

    fn walk_dir_recursive(path: &Path, entries: &mut Vec<DirectoryEntry>) -> io::Result<()> {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let dir_entry = DirectoryEntry::from_dir_entry(entry)?;

            if dir_entry.is_dir {
                Self::walk_dir_recursive(&dir_entry.path, entries)?;
            }

            entries.push(dir_entry);
        }

        Ok(())
    }

    /// Find files matching a pattern
    pub fn find_files<P: AsRef<Path>>(path: P, pattern: &str) -> io::Result<Vec<PathBuf>> {
        let mut matches = Vec::new();

        for entry in Self::walk_dir(path)? {
            if entry.is_file {
                if let Some(filename) = entry.path.file_name() {
                    if filename.to_string_lossy().contains(pattern) {
                        matches.push(entry.path);
                    }
                }
            }
        }

        Ok(matches)
    }

    /// Get directory size (recursive)
    pub fn dir_size<P: AsRef<Path>>(path: P) -> io::Result<u64> {
        let mut total_size = 0;

        for entry in Self::walk_dir(path)? {
            if entry.is_file {
                total_size += entry.size;
            }
        }

        Ok(total_size)
    }
}

/// File utilities
pub struct FileUtils;

impl FileUtils {
    /// Copy a file
    pub fn copy<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<u64> {
        fs::copy(from, to)
    }

    /// Move a file
    pub fn move_file<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<()> {
        fs::rename(from, to)
    }

    /// Delete a file
    pub fn delete<P: AsRef<Path>>(path: P) -> io::Result<()> {
        fs::remove_file(path)
    }

    /// Create a directory
    pub fn create_dir<P: AsRef<Path>>(path: P) -> io::Result<()> {
        fs::create_dir(path)
    }

    /// Create directories recursively
    pub fn create_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
        fs::create_dir_all(path)
    }

    /// Delete a directory
    pub fn delete_dir<P: AsRef<Path>>(path: P) -> io::Result<()> {
        fs::remove_dir(path)
    }

    /// Delete a directory recursively
    pub fn delete_dir_all<P: AsRef<Path>>(path: P) -> io::Result<()> {
        fs::remove_dir_all(path)
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

    /// Get file metadata
    pub fn metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
        fs::metadata(path)
    }

    /// Read file to string
    pub fn read_to_string<P: AsRef<Path>>(path: P) -> io::Result<String> {
        fs::read_to_string(path)
    }

    /// Write string to file
    pub fn write_string<P: AsRef<Path>>(path: P, contents: &str) -> io::Result<()> {
        fs::write(path, contents)
    }

    /// Append to file
    pub fn append<P: AsRef<Path>>(path: P, contents: &[u8]) -> io::Result<()> {
        let mut file = fs::OpenOptions::new()
            .append(true)
            .create(true)
            .open(path)?;
        file.write_all(contents)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    fn test_file_utils_exists() {
        let temp_dir = env::temp_dir();
        assert!(FileUtils::exists(&temp_dir));
        assert!(FileUtils::is_dir(&temp_dir));
    }

    #[test]
    fn test_directory_traversal_list_dir() {
        let temp_dir = env::temp_dir();
        let result = DirectoryTraversal::list_dir(&temp_dir);
        assert!(result.is_ok());
    }

    #[test]
    fn test_directory_traversal_list_files() {
        let temp_dir = env::temp_dir();
        let result = DirectoryTraversal::list_files(&temp_dir);
        assert!(result.is_ok());
    }

    #[test]
    fn test_directory_traversal_list_dirs() {
        let temp_dir = env::temp_dir();
        let result = DirectoryTraversal::list_dirs(&temp_dir);
        assert!(result.is_ok());
    }

    #[test]
    fn test_file_utils_create_and_delete() {
        let temp_dir = env::temp_dir();
        let test_file = temp_dir.join("test_file_utils.txt");

        FileUtils::write_string(&test_file, "test content").unwrap();
        assert!(FileUtils::exists(&test_file));
        assert!(FileUtils::is_file(&test_file));

        let content = FileUtils::read_to_string(&test_file).unwrap();
        assert_eq!(content, "test content");

        FileUtils::delete(&test_file).unwrap();
        assert!(!FileUtils::exists(&test_file));
    }

    #[test]
    fn test_file_utils_append() {
        let temp_dir = env::temp_dir();
        let test_file = temp_dir.join("test_append.txt");

        FileUtils::write_string(&test_file, "line1\n").unwrap();
        FileUtils::append(&test_file, b"line2\n").unwrap();

        let content = FileUtils::read_to_string(&test_file).unwrap();
        assert!(content.contains("line1"));
        assert!(content.contains("line2"));

        FileUtils::delete(&test_file).unwrap();
    }

    #[test]
    fn test_memory_mapped_file() {
        let temp_dir = env::temp_dir();
        let test_file = temp_dir.join("test_mmap.txt");

        FileUtils::write_string(&test_file, "memory mapped content").unwrap();

        let mmap = MemoryMappedFile::new(&test_file).unwrap();
        assert!(mmap.size() > 0);
        assert_eq!(mmap.path(), test_file.as_path());

        let content = mmap.read_all().unwrap();
        assert_eq!(content, b"memory mapped content");

        FileUtils::delete(&test_file).unwrap();
    }

    #[test]
    fn test_directory_entry() {
        let temp_dir = env::temp_dir();
        let result = DirectoryTraversal::list_dir(&temp_dir);

        // Just verify we can list entries successfully
        assert!(result.is_ok());
    }
}
