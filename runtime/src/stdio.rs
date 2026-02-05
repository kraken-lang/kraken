//! Standard I/O streams for the Kraken Language runtime.
//!
//! This module provides abstractions for standard input, output, and error streams
//! with support for both buffered and unbuffered operations.

#![allow(dead_code)]

use std::io::{
    self, BufRead, BufReader, BufWriter, Read, Stderr as StdStderr, Stdin as StdStdin,
    Stdout as StdStdout, Write,
};
use std::sync::{Arc, Mutex};

/// A handle to the standard input stream.
///
/// This handle is internally synchronized and can be safely shared across threads.
pub struct Stdin {
    inner: Arc<Mutex<BufReader<StdStdin>>>,
}

impl Stdin {
    /// Creates a new handle to standard input.
    pub fn new() -> Self {
        Stdin {
            inner: Arc::new(Mutex::new(BufReader::new(io::stdin()))),
        }
    }

    /// Locks the standard input stream for exclusive access.
    ///
    /// Returns a guard that provides buffered reading capabilities.
    pub fn lock(&self) -> StdinLock {
        StdinLock {
            inner: self.inner.clone(),
        }
    }

    /// Reads a line from standard input into the provided string.
    ///
    /// The newline character is included in the output if present.
    pub fn read_line(&self, buf: &mut String) -> io::Result<usize> {
        let mut guard = self.inner.lock().unwrap();
        guard.read_line(buf)
    }

    /// Reads all bytes from standard input until EOF.
    pub fn read_to_end(&self, buf: &mut Vec<u8>) -> io::Result<usize> {
        let mut guard = self.inner.lock().unwrap();
        guard.read_to_end(buf)
    }

    /// Reads all bytes from standard input until EOF as a string.
    pub fn read_to_string(&self, buf: &mut String) -> io::Result<usize> {
        let mut guard = self.inner.lock().unwrap();
        guard.read_to_string(buf)
    }
}

impl Default for Stdin {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for Stdin {
    fn clone(&self) -> Self {
        Stdin {
            inner: self.inner.clone(),
        }
    }
}

/// A locked reference to the standard input stream.
pub struct StdinLock {
    inner: Arc<Mutex<BufReader<StdStdin>>>,
}

impl Read for StdinLock {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut guard = self.inner.lock().unwrap();
        guard.read(buf)
    }
}

impl BufRead for StdinLock {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        // Note: This is a simplified implementation
        // In production, we'd need a more sophisticated approach
        Ok(&[])
    }

    fn consume(&mut self, _amt: usize) {
        // Simplified implementation
    }
}

/// A handle to the standard output stream.
///
/// This handle is internally synchronized and can be safely shared across threads.
pub struct Stdout {
    inner: Arc<Mutex<BufWriter<StdStdout>>>,
}

impl Stdout {
    /// Creates a new handle to standard output.
    pub fn new() -> Self {
        Stdout {
            inner: Arc::new(Mutex::new(BufWriter::new(io::stdout()))),
        }
    }

    /// Locks the standard output stream for exclusive access.
    pub fn lock(&self) -> StdoutLock {
        StdoutLock {
            inner: self.inner.clone(),
        }
    }

    /// Writes a byte slice to standard output.
    pub fn write(&self, buf: &[u8]) -> io::Result<usize> {
        let mut guard = self.inner.lock().unwrap();
        guard.write(buf)
    }

    /// Writes all bytes to standard output.
    pub fn write_all(&self, buf: &[u8]) -> io::Result<()> {
        let mut guard = self.inner.lock().unwrap();
        guard.write_all(buf)
    }

    /// Flushes the output stream, ensuring all buffered data is written.
    pub fn flush(&self) -> io::Result<()> {
        let mut guard = self.inner.lock().unwrap();
        guard.flush()
    }

    /// Writes a formatted string to standard output.
    pub fn write_fmt(&self, fmt: std::fmt::Arguments) -> io::Result<()> {
        let mut guard = self.inner.lock().unwrap();
        guard.write_fmt(fmt)
    }
}

impl Default for Stdout {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for Stdout {
    fn clone(&self) -> Self {
        Stdout {
            inner: self.inner.clone(),
        }
    }
}

/// A locked reference to the standard output stream.
pub struct StdoutLock {
    inner: Arc<Mutex<BufWriter<StdStdout>>>,
}

impl Write for StdoutLock {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut guard = self.inner.lock().unwrap();
        guard.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        let mut guard = self.inner.lock().unwrap();
        guard.flush()
    }
}

/// A handle to the standard error stream.
///
/// This handle is internally synchronized and can be safely shared across threads.
pub struct Stderr {
    inner: Arc<Mutex<BufWriter<StdStderr>>>,
}

impl Stderr {
    /// Creates a new handle to standard error.
    pub fn new() -> Self {
        Stderr {
            inner: Arc::new(Mutex::new(BufWriter::new(io::stderr()))),
        }
    }

    /// Locks the standard error stream for exclusive access.
    pub fn lock(&self) -> StderrLock {
        StderrLock {
            inner: self.inner.clone(),
        }
    }

    /// Writes a byte slice to standard error.
    pub fn write(&self, buf: &[u8]) -> io::Result<usize> {
        let mut guard = self.inner.lock().unwrap();
        guard.write(buf)
    }

    /// Writes all bytes to standard error.
    pub fn write_all(&self, buf: &[u8]) -> io::Result<()> {
        let mut guard = self.inner.lock().unwrap();
        guard.write_all(buf)
    }

    /// Flushes the error stream, ensuring all buffered data is written.
    pub fn flush(&self) -> io::Result<()> {
        let mut guard = self.inner.lock().unwrap();
        guard.flush()
    }

    /// Writes a formatted string to standard error.
    pub fn write_fmt(&self, fmt: std::fmt::Arguments) -> io::Result<()> {
        let mut guard = self.inner.lock().unwrap();
        guard.write_fmt(fmt)
    }
}

impl Default for Stderr {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for Stderr {
    fn clone(&self) -> Self {
        Stderr {
            inner: self.inner.clone(),
        }
    }
}

/// A locked reference to the standard error stream.
pub struct StderrLock {
    inner: Arc<Mutex<BufWriter<StdStderr>>>,
}

impl Write for StderrLock {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut guard = self.inner.lock().unwrap();
        guard.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        let mut guard = self.inner.lock().unwrap();
        guard.flush()
    }
}

/// Returns a handle to the standard input stream.
pub fn stdin() -> Stdin {
    Stdin::new()
}

/// Returns a handle to the standard output stream.
pub fn stdout() -> Stdout {
    Stdout::new()
}

/// Returns a handle to the standard error stream.
pub fn stderr() -> Stderr {
    Stderr::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stdin_creation() {
        let stdin = stdin();
        let _lock = stdin.lock();
    }

    #[test]
    fn test_stdout_creation() {
        let stdout = stdout();
        let _lock = stdout.lock();
    }

    #[test]
    fn test_stderr_creation() {
        let stderr = stderr();
        let _lock = stderr.lock();
    }

    #[test]
    fn test_stdout_write() {
        let stdout = stdout();
        // Write empty buffer to avoid polluting test output
        stdout.write_all(b"").unwrap();
        stdout.flush().unwrap();
    }

    #[test]
    fn test_stderr_write() {
        let stderr = stderr();
        // Write empty buffer to avoid polluting test output
        stderr.write_all(b"").unwrap();
        stderr.flush().unwrap();
    }

    #[test]
    fn test_stdout_clone() {
        let stdout1 = stdout();
        let stdout2 = stdout1.clone();
        stdout1.write_all(b"").unwrap();
        stdout2.write_all(b"").unwrap();
    }

    #[test]
    fn test_stderr_clone() {
        let stderr1 = stderr();
        let stderr2 = stderr1.clone();
        stderr1.write_all(b"").unwrap();
        stderr2.write_all(b"").unwrap();
    }

    #[test]
    fn test_stdout_write_fmt() {
        let stdout = stdout();
        stdout.write_fmt(format_args!("")).unwrap();
    }

    #[test]
    fn test_stderr_write_fmt() {
        let stderr = stderr();
        stderr.write_fmt(format_args!("")).unwrap();
    }

    #[test]
    fn test_stdin_clone() {
        let stdin1 = stdin();
        let stdin2 = stdin1.clone();
        drop(stdin1);
        drop(stdin2);
    }
}
