//! File system monitoring and watching.
//!
//! Provides cross-platform file system event monitoring capabilities.

#![allow(dead_code)]

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime};

/// File system event types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileEvent {
    /// File was created
    Created,
    /// File was modified
    Modified,
    /// File was deleted
    Deleted,
    /// File was renamed
    Renamed,
    /// File metadata changed
    MetadataChanged,
}

/// File watcher configuration
#[derive(Debug, Clone)]
pub struct WatcherConfig {
    /// Poll interval for checking file changes
    pub poll_interval: Duration,
    /// Whether to watch subdirectories recursively
    pub recursive: bool,
    /// File patterns to watch (e.g., "*.rs", "*.txt")
    pub patterns: Vec<String>,
}

impl Default for WatcherConfig {
    fn default() -> Self {
        Self {
            poll_interval: Duration::from_secs(1),
            recursive: false,
            patterns: vec!["*".to_string()],
        }
    }
}

/// File watcher for monitoring file system changes
pub struct FileWatcher {
    watched_paths: Arc<Mutex<HashMap<PathBuf, FileState>>>,
    config: WatcherConfig,
}

#[derive(Debug, Clone)]
struct FileState {
    modified_time: SystemTime,
    size: u64,
    exists: bool,
}

impl FileWatcher {
    /// Create a new file watcher with default configuration
    pub fn new() -> Self {
        Self {
            watched_paths: Arc::new(Mutex::new(HashMap::new())),
            config: WatcherConfig::default(),
        }
    }

    /// Create a new file watcher with custom configuration
    pub fn with_config(config: WatcherConfig) -> Self {
        Self {
            watched_paths: Arc::new(Mutex::new(HashMap::new())),
            config,
        }
    }

    /// Add a path to watch
    pub fn watch<P: AsRef<Path>>(&mut self, path: P) -> Result<(), String> {
        let path = path.as_ref().to_path_buf();
        
        if !path.exists() {
            return Err(format!("Path does not exist: {}", path.display()));
        }

        let metadata = std::fs::metadata(&path)
            .map_err(|e| format!("Failed to read metadata: {e}"))?;

        let state = FileState {
            modified_time: metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH),
            size: metadata.len(),
            exists: true,
        };

        self.watched_paths.lock().unwrap().insert(path, state);
        Ok(())
    }

    /// Remove a path from watching
    pub fn unwatch<P: AsRef<Path>>(&mut self, path: P) {
        let path = path.as_ref().to_path_buf();
        self.watched_paths.lock().unwrap().remove(&path);
    }

    /// Check for file system events
    pub fn poll_events(&self) -> Vec<(PathBuf, FileEvent)> {
        let mut events = Vec::new();
        let mut watched = self.watched_paths.lock().unwrap();

        for (path, old_state) in watched.iter_mut() {
            if !path.exists() {
                if old_state.exists {
                    events.push((path.clone(), FileEvent::Deleted));
                    old_state.exists = false;
                }
                continue;
            }

            if !old_state.exists {
                events.push((path.clone(), FileEvent::Created));
                old_state.exists = true;
            }

            if let Ok(metadata) = std::fs::metadata(path) {
                let modified = metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH);
                let size = metadata.len();

                if modified != old_state.modified_time || size != old_state.size {
                    events.push((path.clone(), FileEvent::Modified));
                    old_state.modified_time = modified;
                    old_state.size = size;
                }
            }
        }

        events
    }

    /// Get the list of watched paths
    pub fn watched_paths(&self) -> Vec<PathBuf> {
        self.watched_paths
            .lock()
            .unwrap()
            .keys()
            .cloned()
            .collect()
    }

    /// Clear all watched paths
    pub fn clear(&mut self) {
        self.watched_paths.lock().unwrap().clear();
    }
}

impl Default for FileWatcher {
    fn default() -> Self {
        Self::new()
    }
}

/// Directory watcher for monitoring directory changes
pub struct DirectoryWatcher {
    watcher: FileWatcher,
}

impl DirectoryWatcher {
    /// Create a new directory watcher
    pub fn new() -> Self {
        Self {
            watcher: FileWatcher::new(),
        }
    }

    /// Watch a directory for changes
    pub fn watch_directory<P: AsRef<Path>>(&mut self, path: P, recursive: bool) -> Result<(), String> {
        let path = path.as_ref();
        
        if !path.is_dir() {
            return Err(format!("Path is not a directory: {}", path.display()));
        }

        self.watcher.watch(path)?;

        if recursive {
            self.watch_subdirectories(path)?;
        }

        Ok(())
    }

    fn watch_subdirectories(&mut self, path: &Path) -> Result<(), String> {
        if let Ok(entries) = std::fs::read_dir(path) {
            for entry in entries.flatten() {
                if let Ok(metadata) = entry.metadata() {
                    if metadata.is_dir() {
                        self.watcher.watch(entry.path())?;
                        self.watch_subdirectories(&entry.path())?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Poll for directory events
    pub fn poll_events(&self) -> Vec<(PathBuf, FileEvent)> {
        self.watcher.poll_events()
    }

    /// Clear all watched directories
    pub fn clear(&mut self) {
        self.watcher.clear();
    }
}

impl Default for DirectoryWatcher {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io::Write;

    #[test]
    fn test_file_watcher_creation() {
        let watcher = FileWatcher::new();
        assert_eq!(watcher.watched_paths().len(), 0);
    }

    #[test]
    fn test_watch_file() {
        let temp_file = std::env::temp_dir().join("test_watch.txt");
        fs::write(&temp_file, "test").unwrap();

        let mut watcher = FileWatcher::new();
        assert!(watcher.watch(&temp_file).is_ok());
        assert_eq!(watcher.watched_paths().len(), 1);

        fs::remove_file(&temp_file).ok();
    }

    #[test]
    fn test_watch_nonexistent_file() {
        let mut watcher = FileWatcher::new();
        let result = watcher.watch("/nonexistent/file.txt");
        assert!(result.is_err());
    }

    #[test]
    fn test_unwatch_file() {
        let temp_file = std::env::temp_dir().join("test_unwatch.txt");
        fs::write(&temp_file, "test").unwrap();

        let mut watcher = FileWatcher::new();
        watcher.watch(&temp_file).unwrap();
        assert_eq!(watcher.watched_paths().len(), 1);

        watcher.unwatch(&temp_file);
        assert_eq!(watcher.watched_paths().len(), 0);

        fs::remove_file(&temp_file).ok();
    }

    #[test]
    fn test_detect_file_modification() {
        let temp_file = std::env::temp_dir().join("test_modify.txt");
        fs::write(&temp_file, "initial").unwrap();

        let mut watcher = FileWatcher::new();
        watcher.watch(&temp_file).unwrap();

        // Initial poll should have no events
        let events = watcher.poll_events();
        assert_eq!(events.len(), 0);

        // Modify the file
        std::thread::sleep(Duration::from_millis(100));
        let mut file = fs::OpenOptions::new()
            .append(true)
            .open(&temp_file)
            .unwrap();
        file.write_all(b" modified").unwrap();
        drop(file);

        // Poll should detect modification
        std::thread::sleep(Duration::from_millis(100));
        let events = watcher.poll_events();
        assert!(!events.is_empty());
        assert_eq!(events[0].1, FileEvent::Modified);

        fs::remove_file(&temp_file).ok();
    }

    #[test]
    fn test_detect_file_deletion() {
        let temp_file = std::env::temp_dir().join("test_delete.txt");
        fs::write(&temp_file, "test").unwrap();

        let mut watcher = FileWatcher::new();
        watcher.watch(&temp_file).unwrap();

        // Delete the file
        fs::remove_file(&temp_file).unwrap();

        // Poll should detect deletion
        let events = watcher.poll_events();
        assert!(!events.is_empty());
        assert_eq!(events[0].1, FileEvent::Deleted);
    }

    #[test]
    fn test_clear_watched_paths() {
        let temp_file = std::env::temp_dir().join("test_clear.txt");
        fs::write(&temp_file, "test").unwrap();

        let mut watcher = FileWatcher::new();
        watcher.watch(&temp_file).unwrap();
        assert_eq!(watcher.watched_paths().len(), 1);

        watcher.clear();
        assert_eq!(watcher.watched_paths().len(), 0);

        fs::remove_file(&temp_file).ok();
    }

    #[test]
    fn test_directory_watcher() {
        let temp_dir = std::env::temp_dir().join("test_dir_watch");
        fs::create_dir_all(&temp_dir).ok();

        let mut watcher = DirectoryWatcher::new();
        assert!(watcher.watch_directory(&temp_dir, false).is_ok());

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_watcher_config() {
        let config = WatcherConfig {
            poll_interval: Duration::from_millis(500),
            recursive: true,
            patterns: vec!["*.rs".to_string()],
        };

        let watcher = FileWatcher::with_config(config.clone());
        assert_eq!(watcher.config.poll_interval, config.poll_interval);
        assert_eq!(watcher.config.recursive, config.recursive);
    }
}
