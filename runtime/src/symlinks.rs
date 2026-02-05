//! Symbolic link creation and resolution utilities.

#![allow(dead_code)]

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[cfg(unix)]
use std::os::unix::fs as unix_fs;

#[cfg(windows)]
use std::os::windows::fs as windows_fs;

/// Symlink utilities
pub struct SymlinkUtils;

impl SymlinkUtils {
    /// Create a symbolic link to a file
    #[cfg(unix)]
    pub fn create_file<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
        unix_fs::symlink(original, link)
    }

    /// Create a symbolic link to a file (Windows)
    #[cfg(windows)]
    pub fn create_file<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
        windows_fs::symlink_file(original, link)
    }

    /// Create a symbolic link to a directory
    #[cfg(unix)]
    pub fn create_dir<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
        unix_fs::symlink(original, link)
    }

    /// Create a symbolic link to a directory (Windows)
    #[cfg(windows)]
    pub fn create_dir<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
        windows_fs::symlink_dir(original, link)
    }

    /// Create a symbolic link (auto-detect file or directory)
    pub fn create<P: AsRef<Path>, Q: AsRef<Path>>(original: P, link: Q) -> io::Result<()> {
        let original_path = original.as_ref();

        if original_path.is_dir() {
            Self::create_dir(original, link)
        } else {
            Self::create_file(original, link)
        }
    }

    /// Read the target of a symbolic link
    pub fn read_link<P: AsRef<Path>>(link: P) -> io::Result<PathBuf> {
        fs::read_link(link)
    }

    /// Resolve a symbolic link to its final target (following all links)
    pub fn canonicalize<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
        fs::canonicalize(path)
    }

    /// Check if a path is a symbolic link
    pub fn is_symlink<P: AsRef<Path>>(path: P) -> bool {
        fs::symlink_metadata(path)
            .map(|m| m.is_symlink())
            .unwrap_or(false)
    }

    /// Remove a symbolic link
    pub fn remove<P: AsRef<Path>>(link: P) -> io::Result<()> {
        let link_path = link.as_ref();

        if Self::is_symlink(link_path) {
            #[cfg(unix)]
            {
                fs::remove_file(link_path)
            }
            #[cfg(windows)]
            {
                if link_path.is_dir() {
                    fs::remove_dir(link_path)
                } else {
                    fs::remove_file(link_path)
                }
            }
        } else {
            Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Path is not a symbolic link",
            ))
        }
    }

    /// Get the target of a symlink without resolving further links
    pub fn read_link_shallow<P: AsRef<Path>>(link: P) -> io::Result<PathBuf> {
        fs::read_link(link)
    }

    /// Check if a symlink is broken (target doesn't exist)
    pub fn is_broken<P: AsRef<Path>>(link: P) -> io::Result<bool> {
        let link_path = link.as_ref();

        if !Self::is_symlink(link_path) {
            return Ok(false);
        }

        let target = Self::read_link(link_path)?;
        Ok(!target.exists())
    }

    /// Resolve symlink chain and return all intermediate links
    pub fn resolve_chain<P: AsRef<Path>>(path: P) -> io::Result<Vec<PathBuf>> {
        let mut chain = Vec::new();
        let mut current = path.as_ref().to_path_buf();
        let mut visited = std::collections::HashSet::new();

        while Self::is_symlink(&current) {
            if !visited.insert(current.clone()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Circular symlink detected",
                ));
            }

            chain.push(current.clone());
            current = Self::read_link(&current)?;

            if !current.is_absolute() {
                if let Some(parent) = chain.last().and_then(|p| p.parent()) {
                    current = parent.join(current);
                }
            }
        }

        chain.push(current);
        Ok(chain)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs::File;

    #[test]
    fn test_create_and_read_symlink() {
        let temp_dir = env::temp_dir();
        let original = temp_dir.join("symlink_original.txt");
        let link = temp_dir.join("symlink_link.txt");

        File::create(&original).unwrap();

        let result = SymlinkUtils::create_file(&original, &link);

        if result.is_ok() {
            assert!(SymlinkUtils::is_symlink(&link));

            let target = SymlinkUtils::read_link(&link).unwrap();
            assert_eq!(target, original);

            SymlinkUtils::remove(&link).unwrap();
        }

        fs::remove_file(&original).unwrap();
    }

    #[test]
    fn test_create_dir_symlink() {
        let temp_dir = env::temp_dir();
        let original = temp_dir.join("symlink_original_dir");
        let link = temp_dir.join("symlink_link_dir");

        fs::create_dir(&original).unwrap();

        let result = SymlinkUtils::create_dir(&original, &link);

        if result.is_ok() {
            assert!(SymlinkUtils::is_symlink(&link));

            SymlinkUtils::remove(&link).unwrap();
        }

        fs::remove_dir(&original).unwrap();
    }

    #[test]
    fn test_canonicalize() {
        let temp_dir = env::temp_dir();
        let canonical = SymlinkUtils::canonicalize(&temp_dir).unwrap();
        assert!(canonical.is_absolute());
    }

    #[test]
    fn test_is_symlink_false() {
        let temp_dir = env::temp_dir();
        assert!(!SymlinkUtils::is_symlink(&temp_dir));
    }

    #[test]
    fn test_broken_symlink() {
        let temp_dir = env::temp_dir();
        let original = temp_dir.join("symlink_temp.txt");
        let link = temp_dir.join("symlink_broken.txt");

        File::create(&original).unwrap();

        let result = SymlinkUtils::create_file(&original, &link);

        if result.is_ok() {
            fs::remove_file(&original).unwrap();

            let is_broken = SymlinkUtils::is_broken(&link).unwrap();
            assert!(is_broken);

            SymlinkUtils::remove(&link).unwrap();
        } else {
            fs::remove_file(&original).unwrap();
        }
    }

    #[test]
    fn test_auto_create() {
        let temp_dir = env::temp_dir();
        let original = temp_dir.join("symlink_auto.txt");
        let link = temp_dir.join("symlink_auto_link.txt");

        File::create(&original).unwrap();

        let result = SymlinkUtils::create(&original, &link);

        if result.is_ok() {
            assert!(SymlinkUtils::is_symlink(&link));
            SymlinkUtils::remove(&link).unwrap();
        }

        fs::remove_file(&original).unwrap();
    }
}
