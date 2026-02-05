//! Glob pattern matching for file paths.

#![allow(dead_code)]

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// Glob pattern matcher
pub struct GlobPattern {
    pattern: String,
}

impl GlobPattern {
    /// Create a new glob pattern
    pub fn new(pattern: &str) -> Self {
        Self {
            pattern: pattern.to_string(),
        }
    }

    /// Check if a path matches the pattern
    pub fn matches<P: AsRef<Path>>(&self, path: P) -> bool {
        let path_str = path.as_ref().to_string_lossy();
        Self::glob_match(&self.pattern, &path_str)
    }

    /// Find all files matching the pattern in a directory
    pub fn find<P: AsRef<Path>>(&self, base_dir: P) -> io::Result<Vec<PathBuf>> {
        let mut matches = Vec::new();
        self.find_recursive(base_dir.as_ref(), &mut matches)?;
        Ok(matches)
    }

    /// Find all files matching the pattern (non-recursive)
    pub fn find_shallow<P: AsRef<Path>>(&self, base_dir: P) -> io::Result<Vec<PathBuf>> {
        let mut matches = Vec::new();

        for entry in fs::read_dir(base_dir)? {
            let entry = entry?;
            let path = entry.path();

            if self.matches(&path) {
                matches.push(path);
            }
        }

        Ok(matches)
    }

    fn find_recursive(&self, dir: &Path, matches: &mut Vec<PathBuf>) -> io::Result<()> {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if self.matches(&path) {
                matches.push(path.clone());
            }

            if path.is_dir() {
                self.find_recursive(&path, matches)?;
            }
        }

        Ok(())
    }

    fn glob_match(pattern: &str, text: &str) -> bool {
        Self::glob_match_impl(pattern.as_bytes(), text.as_bytes())
    }

    fn glob_match_impl(pattern: &[u8], text: &[u8]) -> bool {
        let mut p_idx = 0;
        let mut t_idx = 0;
        let mut star_idx = None;
        let mut match_idx = 0;

        while t_idx < text.len() {
            if p_idx < pattern.len() {
                match pattern[p_idx] {
                    b'*' => {
                        star_idx = Some(p_idx);
                        match_idx = t_idx;
                        p_idx += 1;
                        continue;
                    }
                    b'?' => {
                        p_idx += 1;
                        t_idx += 1;
                        continue;
                    }
                    c if c == text[t_idx] => {
                        p_idx += 1;
                        t_idx += 1;
                        continue;
                    }
                    _ => {}
                }
            }

            if let Some(star) = star_idx {
                p_idx = star + 1;
                match_idx += 1;
                t_idx = match_idx;
            } else {
                return false;
            }
        }

        while p_idx < pattern.len() && pattern[p_idx] == b'*' {
            p_idx += 1;
        }

        p_idx == pattern.len()
    }
}

/// Glob utilities
pub struct GlobUtils;

impl GlobUtils {
    /// Find files matching a glob pattern
    pub fn glob<P: AsRef<Path>>(pattern: &str, base_dir: P) -> io::Result<Vec<PathBuf>> {
        let glob = GlobPattern::new(pattern);
        glob.find(base_dir)
    }

    /// Find files matching a glob pattern (non-recursive)
    pub fn glob_shallow<P: AsRef<Path>>(pattern: &str, base_dir: P) -> io::Result<Vec<PathBuf>> {
        let glob = GlobPattern::new(pattern);
        glob.find_shallow(base_dir)
    }

    /// Check if a path matches a glob pattern
    pub fn matches<P: AsRef<Path>>(pattern: &str, path: P) -> bool {
        let glob = GlobPattern::new(pattern);
        glob.matches(path)
    }

    /// Find files with a specific extension
    pub fn find_by_extension<P: AsRef<Path>>(
        base_dir: P,
        extension: &str,
    ) -> io::Result<Vec<PathBuf>> {
        let pattern = format!("*.{extension}");
        Self::glob(&pattern, base_dir)
    }

    /// Find files matching multiple patterns
    pub fn glob_multi<P: AsRef<Path>>(patterns: &[&str], base_dir: P) -> io::Result<Vec<PathBuf>> {
        let mut all_matches = Vec::new();
        let base = base_dir.as_ref();

        for pattern in patterns {
            let mut matches = Self::glob(pattern, base)?;
            all_matches.append(&mut matches);
        }

        all_matches.sort();
        all_matches.dedup();
        Ok(all_matches)
    }

    /// Escape special glob characters in a string
    pub fn escape(text: &str) -> String {
        text.chars()
            .map(|c| match c {
                '*' | '?' | '[' | ']' => format!("[{c}]"),
                _ => c.to_string(),
            })
            .collect()
    }
}

/// Builder for glob patterns with options
pub struct GlobBuilder {
    pattern: String,
    case_insensitive: bool,
    follow_symlinks: bool,
}

impl GlobBuilder {
    /// Create a new glob builder
    pub fn new(pattern: &str) -> Self {
        Self {
            pattern: pattern.to_string(),
            case_insensitive: false,
            follow_symlinks: true,
        }
    }

    /// Set case insensitive matching
    pub fn case_insensitive(mut self, value: bool) -> Self {
        self.case_insensitive = value;
        self
    }

    /// Set whether to follow symlinks
    pub fn follow_symlinks(mut self, value: bool) -> Self {
        self.follow_symlinks = value;
        self
    }

    /// Build and execute the glob pattern
    pub fn glob<P: AsRef<Path>>(self, base_dir: P) -> io::Result<Vec<PathBuf>> {
        let pattern = if self.case_insensitive {
            self.pattern.to_lowercase()
        } else {
            self.pattern.clone()
        };

        let glob = GlobPattern::new(&pattern);
        let mut matches = Vec::new();
        self.find_recursive(base_dir.as_ref(), &glob, &mut matches)?;
        Ok(matches)
    }

    fn find_recursive(
        &self,
        dir: &Path,
        glob: &GlobPattern,
        matches: &mut Vec<PathBuf>,
    ) -> io::Result<()> {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            let filename = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

            let match_str = if self.case_insensitive {
                filename.to_lowercase()
            } else {
                filename.to_string()
            };

            if glob.matches(Path::new(&match_str)) {
                matches.push(path.clone());
            }

            if path.is_dir() && (self.follow_symlinks || !path.is_symlink()) {
                self.find_recursive(&path, glob, matches)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs::File;

    #[test]
    fn test_glob_match_simple() {
        let glob = GlobPattern::new("*.txt");
        assert!(glob.matches("file.txt"));
        assert!(glob.matches("test.txt"));
        assert!(!glob.matches("file.rs"));
    }

    #[test]
    fn test_glob_match_question() {
        let glob = GlobPattern::new("file?.txt");
        assert!(glob.matches("file1.txt"));
        assert!(glob.matches("fileA.txt"));
        assert!(!glob.matches("file.txt"));
        assert!(!glob.matches("file12.txt"));
    }

    #[test]
    fn test_glob_match_multiple_stars() {
        let glob = GlobPattern::new("*test*.txt");
        assert!(glob.matches("test.txt"));
        assert!(glob.matches("mytest.txt"));
        assert!(glob.matches("test_file.txt"));
        assert!(glob.matches("my_test_file.txt"));
    }

    #[test]
    fn test_glob_match_exact() {
        let glob = GlobPattern::new("exact.txt");
        assert!(glob.matches("exact.txt"));
        assert!(!glob.matches("not_exact.txt"));
    }

    #[test]
    fn test_glob_find() {
        let temp_dir = env::temp_dir().join("glob_test");
        fs::create_dir_all(&temp_dir).unwrap();

        File::create(temp_dir.join("test1.txt")).unwrap();
        File::create(temp_dir.join("test2.txt")).unwrap();
        File::create(temp_dir.join("test.rs")).unwrap();

        let glob = GlobPattern::new("*.txt");
        let matches = glob.find(&temp_dir).unwrap();

        assert!(matches.iter().any(|p| p.ends_with("test1.txt")));
        assert!(matches.iter().any(|p| p.ends_with("test2.txt")));
        assert!(!matches.iter().any(|p| p.ends_with("test.rs")));

        fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_glob_utils_matches() {
        assert!(GlobUtils::matches("*.txt", "file.txt"));
        assert!(!GlobUtils::matches("*.txt", "file.rs"));
    }

    #[test]
    fn test_glob_utils_escape() {
        let escaped = GlobUtils::escape("file*.txt");
        assert_eq!(escaped, "file[*].txt");
    }

    #[test]
    fn test_glob_builder() {
        use std::time::SystemTime;
        let unique_id = SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let temp_dir = env::temp_dir().join(format!("glob_builder_test_{unique_id}"));
        fs::create_dir_all(&temp_dir).unwrap();

        File::create(temp_dir.join("file1.txt")).unwrap();
        File::create(temp_dir.join("file2.txt")).unwrap();
        File::create(temp_dir.join("file3.rs")).unwrap();

        let matches = GlobBuilder::new("file*.txt")
            .follow_symlinks(true)
            .glob(&temp_dir)
            .unwrap();

        assert_eq!(matches.len(), 2);

        fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_glob_shallow() {
        let temp_dir = env::temp_dir().join("glob_shallow_test");
        fs::create_dir_all(&temp_dir).unwrap();
        fs::create_dir_all(temp_dir.join("subdir")).unwrap();

        File::create(temp_dir.join("test1.txt")).unwrap();
        File::create(temp_dir.join("subdir").join("test2.txt")).unwrap();

        let glob = GlobPattern::new("*.txt");
        let matches = glob.find_shallow(&temp_dir).unwrap();

        assert_eq!(matches.len(), 1);
        assert!(matches[0].ends_with("test1.txt"));

        fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_glob_multi() {
        let temp_dir = env::temp_dir().join("glob_multi_test");
        fs::create_dir_all(&temp_dir).unwrap();

        File::create(temp_dir.join("test.txt")).unwrap();
        File::create(temp_dir.join("test.rs")).unwrap();
        File::create(temp_dir.join("test.md")).unwrap();

        let matches = GlobUtils::glob_multi(&["*.txt", "*.rs"], &temp_dir).unwrap();

        assert_eq!(matches.len(), 2);

        fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_find_by_extension() {
        let temp_dir = env::temp_dir().join("glob_ext_test");
        fs::create_dir_all(&temp_dir).unwrap();

        File::create(temp_dir.join("test1.txt")).unwrap();
        File::create(temp_dir.join("test2.txt")).unwrap();
        File::create(temp_dir.join("test.rs")).unwrap();

        let matches = GlobUtils::find_by_extension(&temp_dir, "txt").unwrap();

        assert_eq!(matches.len(), 2);

        fs::remove_dir_all(&temp_dir).unwrap();
    }
}
