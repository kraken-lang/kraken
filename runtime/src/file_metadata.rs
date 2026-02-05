//! Enhanced file metadata utilities including permissions, timestamps, and file types.

#![allow(dead_code)]

use std::fs::{self, Metadata};
use std::io;
use std::path::Path;
use std::time::SystemTime;

#[cfg(unix)]
use std::os::unix::fs::{MetadataExt, PermissionsExt};

/// File type information
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    File,
    Directory,
    Symlink,
    Other,
}

/// File permissions wrapper
#[derive(Debug, Clone)]
pub struct FilePermissions {
    pub readonly: bool,
    #[cfg(unix)]
    pub mode: u32,
}

impl FilePermissions {
    /// Create from std::fs::Permissions
    pub fn from_std(perms: std::fs::Permissions) -> Self {
        Self {
            readonly: perms.readonly(),
            #[cfg(unix)]
            mode: perms.mode(),
        }
    }

    /// Check if owner can read
    #[cfg(unix)]
    pub fn owner_read(&self) -> bool {
        self.mode & 0o400 != 0
    }

    /// Check if owner can write
    #[cfg(unix)]
    pub fn owner_write(&self) -> bool {
        self.mode & 0o200 != 0
    }

    /// Check if owner can execute
    #[cfg(unix)]
    pub fn owner_execute(&self) -> bool {
        self.mode & 0o100 != 0
    }

    /// Check if group can read
    #[cfg(unix)]
    pub fn group_read(&self) -> bool {
        self.mode & 0o040 != 0
    }

    /// Check if group can write
    #[cfg(unix)]
    pub fn group_write(&self) -> bool {
        self.mode & 0o020 != 0
    }

    /// Check if group can execute
    #[cfg(unix)]
    pub fn group_execute(&self) -> bool {
        self.mode & 0o010 != 0
    }

    /// Check if others can read
    #[cfg(unix)]
    pub fn others_read(&self) -> bool {
        self.mode & 0o004 != 0
    }

    /// Check if others can write
    #[cfg(unix)]
    pub fn others_write(&self) -> bool {
        self.mode & 0o002 != 0
    }

    /// Check if others can execute
    #[cfg(unix)]
    pub fn others_execute(&self) -> bool {
        self.mode & 0o001 != 0
    }

    /// Get octal representation of permissions (Unix only)
    #[cfg(unix)]
    pub fn as_octal(&self) -> u32 {
        self.mode & 0o777
    }

    /// Create permissions from octal mode (Unix only)
    #[cfg(unix)]
    pub fn from_octal(mode: u32) -> Self {
        Self {
            readonly: mode & 0o200 == 0,
            mode,
        }
    }
}

/// Enhanced file metadata
#[derive(Debug, Clone)]
pub struct EnhancedMetadata {
    pub file_type: FileType,
    pub size: u64,
    pub permissions: FilePermissions,
    pub modified: Option<SystemTime>,
    pub accessed: Option<SystemTime>,
    pub created: Option<SystemTime>,
    #[cfg(unix)]
    pub inode: u64,
    #[cfg(unix)]
    pub nlink: u64,
    #[cfg(unix)]
    pub uid: u32,
    #[cfg(unix)]
    pub gid: u32,
}

impl EnhancedMetadata {
    /// Create from std::fs::Metadata
    pub fn from_std(metadata: Metadata) -> Self {
        let file_type = if metadata.is_file() {
            FileType::File
        } else if metadata.is_dir() {
            FileType::Directory
        } else if metadata.is_symlink() {
            FileType::Symlink
        } else {
            FileType::Other
        };

        Self {
            file_type,
            size: metadata.len(),
            permissions: FilePermissions::from_std(metadata.permissions()),
            modified: metadata.modified().ok(),
            accessed: metadata.accessed().ok(),
            created: metadata.created().ok(),
            #[cfg(unix)]
            inode: metadata.ino(),
            #[cfg(unix)]
            nlink: metadata.nlink(),
            #[cfg(unix)]
            uid: metadata.uid(),
            #[cfg(unix)]
            gid: metadata.gid(),
        }
    }

    /// Check if this is a regular file
    pub fn is_file(&self) -> bool {
        self.file_type == FileType::File
    }

    /// Check if this is a directory
    pub fn is_dir(&self) -> bool {
        self.file_type == FileType::Directory
    }

    /// Check if this is a symlink
    pub fn is_symlink(&self) -> bool {
        self.file_type == FileType::Symlink
    }

    /// Get file size in bytes
    pub fn size(&self) -> u64 {
        self.size
    }

    /// Get file size in human-readable format
    pub fn size_human_readable(&self) -> String {
        const UNITS: &[&str] = &["B", "KB", "MB", "GB", "TB", "PB"];
        let mut size = self.size as f64;
        let mut unit_index = 0;

        while size >= 1024.0 && unit_index < UNITS.len() - 1 {
            size /= 1024.0;
            unit_index += 1;
        }

        if unit_index == 0 {
            format!("{} {}", size as u64, UNITS[unit_index])
        } else {
            format!("{:.2} {}", size, UNITS[unit_index])
        }
    }
}

/// File metadata utilities
pub struct FileMetadataUtils;

impl FileMetadataUtils {
    /// Get enhanced metadata for a file
    pub fn metadata<P: AsRef<Path>>(path: P) -> io::Result<EnhancedMetadata> {
        let metadata = fs::metadata(path)?;
        Ok(EnhancedMetadata::from_std(metadata))
    }

    /// Get enhanced metadata for a file without following symlinks
    pub fn symlink_metadata<P: AsRef<Path>>(path: P) -> io::Result<EnhancedMetadata> {
        let metadata = fs::symlink_metadata(path)?;
        Ok(EnhancedMetadata::from_std(metadata))
    }

    /// Set file permissions (Unix only)
    #[cfg(unix)]
    pub fn set_permissions<P: AsRef<Path>>(path: P, mode: u32) -> io::Result<()> {
        use std::fs::Permissions;
        let perms = Permissions::from_mode(mode);
        fs::set_permissions(path, perms)
    }

    /// Make file readonly
    pub fn set_readonly<P: AsRef<Path>>(path: P, readonly: bool) -> io::Result<()> {
        let metadata = fs::metadata(&path)?;
        let mut perms = metadata.permissions();
        perms.set_readonly(readonly);
        fs::set_permissions(path, perms)
    }

    /// Get file size
    pub fn file_size<P: AsRef<Path>>(path: P) -> io::Result<u64> {
        let metadata = fs::metadata(path)?;
        Ok(metadata.len())
    }

    /// Get file modified time
    pub fn modified_time<P: AsRef<Path>>(path: P) -> io::Result<SystemTime> {
        let metadata = fs::metadata(path)?;
        metadata.modified()
    }

    /// Get file accessed time
    pub fn accessed_time<P: AsRef<Path>>(path: P) -> io::Result<SystemTime> {
        let metadata = fs::metadata(path)?;
        metadata.accessed()
    }

    /// Get file created time
    pub fn created_time<P: AsRef<Path>>(path: P) -> io::Result<SystemTime> {
        let metadata = fs::metadata(path)?;
        metadata.created()
    }

    /// Check if file is readonly
    pub fn is_readonly<P: AsRef<Path>>(path: P) -> io::Result<bool> {
        let metadata = fs::metadata(path)?;
        Ok(metadata.permissions().readonly())
    }

    /// Get file type
    pub fn file_type<P: AsRef<Path>>(path: P) -> io::Result<FileType> {
        let metadata = fs::metadata(path)?;
        Ok(if metadata.is_file() {
            FileType::File
        } else if metadata.is_dir() {
            FileType::Directory
        } else if metadata.is_symlink() {
            FileType::Symlink
        } else {
            FileType::Other
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs::File;

    #[test]
    fn test_file_metadata() {
        let temp_dir = env::temp_dir();
        let test_file = temp_dir.join("test_metadata.txt");

        File::create(&test_file).unwrap();

        let metadata = FileMetadataUtils::metadata(&test_file).unwrap();
        assert!(metadata.is_file());
        assert!(!metadata.is_dir());
        assert!(!metadata.is_symlink());

        fs::remove_file(&test_file).unwrap();
    }

    #[test]
    fn test_directory_metadata() {
        let temp_dir = env::temp_dir();
        let metadata = FileMetadataUtils::metadata(&temp_dir).unwrap();
        assert!(metadata.is_dir());
        assert!(!metadata.is_file());
    }

    #[test]
    fn test_file_size() {
        let temp_dir = env::temp_dir();
        let test_file = temp_dir.join("test_size.txt");

        fs::write(&test_file, "Hello, World!").unwrap();

        let size = FileMetadataUtils::file_size(&test_file).unwrap();
        assert_eq!(size, 13);

        fs::remove_file(&test_file).unwrap();
    }

    #[test]
    fn test_size_human_readable() {
        let metadata = EnhancedMetadata {
            file_type: FileType::File,
            size: 1536,
            permissions: FilePermissions {
                readonly: false,
                #[cfg(unix)]
                mode: 0o644,
            },
            modified: None,
            accessed: None,
            created: None,
            #[cfg(unix)]
            inode: 0,
            #[cfg(unix)]
            nlink: 1,
            #[cfg(unix)]
            uid: 0,
            #[cfg(unix)]
            gid: 0,
        };

        let size_str = metadata.size_human_readable();
        assert!(size_str.contains("KB"));
    }

    #[test]
    fn test_readonly() {
        let temp_dir = env::temp_dir();
        let test_file = temp_dir.join("test_readonly.txt");

        File::create(&test_file).unwrap();

        FileMetadataUtils::set_readonly(&test_file, true).unwrap();
        assert!(FileMetadataUtils::is_readonly(&test_file).unwrap());

        FileMetadataUtils::set_readonly(&test_file, false).unwrap();
        assert!(!FileMetadataUtils::is_readonly(&test_file).unwrap());

        fs::remove_file(&test_file).unwrap();
    }

    #[test]
    fn test_file_times() {
        let temp_dir = env::temp_dir();
        let test_file = temp_dir.join("test_times.txt");

        File::create(&test_file).unwrap();

        let modified = FileMetadataUtils::modified_time(&test_file);
        assert!(modified.is_ok());

        let accessed = FileMetadataUtils::accessed_time(&test_file);
        assert!(accessed.is_ok());

        fs::remove_file(&test_file).unwrap();
    }

    #[test]
    #[cfg(unix)]
    fn test_permissions_unix() {
        let temp_dir = env::temp_dir();
        let test_file = temp_dir.join("test_perms.txt");

        File::create(&test_file).unwrap();

        FileMetadataUtils::set_permissions(&test_file, 0o644).unwrap();

        let metadata = FileMetadataUtils::metadata(&test_file).unwrap();
        assert!(metadata.permissions.owner_read());
        assert!(metadata.permissions.owner_write());
        assert!(!metadata.permissions.owner_execute());

        fs::remove_file(&test_file).unwrap();
    }

    #[test]
    fn test_file_type() {
        let temp_dir = env::temp_dir();
        let file_type = FileMetadataUtils::file_type(&temp_dir).unwrap();
        assert_eq!(file_type, FileType::Directory);
    }
}
