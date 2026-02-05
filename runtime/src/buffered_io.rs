//! Buffered I/O operations for the Kraken Language runtime.
//!
//! This module provides buffered reading and writing capabilities for improved
//! I/O performance by reducing the number of system calls.

#![allow(dead_code)]

use std::fs::File;
use std::io::{self, BufRead, Read, Seek, SeekFrom, Write};

/// Default buffer size for buffered I/O operations (8 KB).
const DEFAULT_BUFFER_SIZE: usize = 8 * 1024;

/// A buffered reader that wraps any `Read` type.
///
/// Buffering reduces the number of system calls by reading data in chunks
/// and serving subsequent reads from an internal buffer.
pub struct BufferedReader<R: Read> {
    inner: R,
    buffer: Vec<u8>,
    pos: usize,
    cap: usize,
}

impl<R: Read> BufferedReader<R> {
    /// Creates a new `BufferedReader` with the default buffer size.
    pub fn new(inner: R) -> Self {
        Self::with_capacity(DEFAULT_BUFFER_SIZE, inner)
    }

    /// Creates a new `BufferedReader` with the specified buffer capacity.
    pub fn with_capacity(capacity: usize, inner: R) -> Self {
        BufferedReader {
            inner,
            buffer: vec![0; capacity],
            pos: 0,
            cap: 0,
        }
    }

    /// Gets a reference to the underlying reader.
    pub fn get_ref(&self) -> &R {
        &self.inner
    }

    /// Gets a mutable reference to the underlying reader.
    pub fn get_mut(&mut self) -> &mut R {
        &mut self.inner
    }

    /// Returns the number of bytes currently in the buffer.
    pub fn buffer_len(&self) -> usize {
        self.cap - self.pos
    }

    /// Consumes the `BufferedReader`, returning the underlying reader.
    pub fn into_inner(self) -> R {
        self.inner
    }

    /// Fills the internal buffer with data from the underlying reader.
    fn fill_buffer(&mut self) -> io::Result<()> {
        self.cap = self.inner.read(&mut self.buffer)?;
        self.pos = 0;
        Ok(())
    }
}

impl<R: Read> Read for BufferedReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        // If buffer is empty, refill it
        if self.pos >= self.cap {
            if buf.len() >= self.buffer.len() {
                // If requested size is larger than buffer, read directly
                return self.inner.read(buf);
            }
            self.fill_buffer()?;
            if self.cap == 0 {
                return Ok(0);
            }
        }

        // Copy from buffer to output
        let available = self.cap - self.pos;
        let to_copy = available.min(buf.len());
        buf[..to_copy].copy_from_slice(&self.buffer[self.pos..self.pos + to_copy]);
        self.pos += to_copy;
        Ok(to_copy)
    }
}

impl<R: Read> BufRead for BufferedReader<R> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        if self.pos >= self.cap {
            self.fill_buffer()?;
        }
        Ok(&self.buffer[self.pos..self.cap])
    }

    fn consume(&mut self, amt: usize) {
        self.pos = (self.pos + amt).min(self.cap);
    }
}

impl<R: Read + Seek> Seek for BufferedReader<R> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        // Discard buffer on seek
        self.pos = 0;
        self.cap = 0;
        self.inner.seek(pos)
    }
}

/// A buffered writer that wraps any `Write` type.
///
/// Buffering reduces the number of system calls by accumulating writes
/// in an internal buffer and flushing when full or explicitly requested.
pub struct BufferedWriter<W: Write> {
    inner: W,
    buffer: Vec<u8>,
    pos: usize,
}

impl<W: Write> BufferedWriter<W> {
    /// Creates a new `BufferedWriter` with the default buffer size.
    pub fn new(inner: W) -> Self {
        Self::with_capacity(DEFAULT_BUFFER_SIZE, inner)
    }

    /// Creates a new `BufferedWriter` with the specified buffer capacity.
    pub fn with_capacity(capacity: usize, inner: W) -> Self {
        BufferedWriter {
            inner,
            buffer: vec![0; capacity],
            pos: 0,
        }
    }

    /// Gets a reference to the underlying writer.
    pub fn get_ref(&self) -> &W {
        &self.inner
    }

    /// Gets a mutable reference to the underlying writer.
    pub fn get_mut(&mut self) -> &mut W {
        &mut self.inner
    }

    /// Returns the number of bytes currently in the buffer.
    pub fn buffer_len(&self) -> usize {
        self.pos
    }

    /// Consumes the `BufferedWriter`, flushing and returning the underlying writer.
    pub fn into_inner(mut self) -> io::Result<W> {
        self.flush()?;
        // Use ManuallyDrop to prevent Drop from running
        let writer = std::mem::ManuallyDrop::new(self);
        // Safety: We've flushed and are consuming self, so it's safe to take ownership
        Ok(unsafe { std::ptr::read(&writer.inner as *const W) })
    }

    /// Flushes the internal buffer to the underlying writer.
    fn flush_buffer(&mut self) -> io::Result<()> {
        if self.pos > 0 {
            self.inner.write_all(&self.buffer[..self.pos])?;
            self.pos = 0;
        }
        Ok(())
    }
}

impl<W: Write> Write for BufferedWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // If write is larger than buffer, flush and write directly
        if buf.len() >= self.buffer.len() {
            self.flush_buffer()?;
            return self.inner.write(buf);
        }

        // If buffer would overflow, flush first
        if self.pos + buf.len() > self.buffer.len() {
            self.flush_buffer()?;
        }

        // Copy to buffer
        self.buffer[self.pos..self.pos + buf.len()].copy_from_slice(buf);
        self.pos += buf.len();
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.flush_buffer()?;
        self.inner.flush()
    }
}

impl<W: Write> Drop for BufferedWriter<W> {
    fn drop(&mut self) {
        let _ = self.flush_buffer();
    }
}

impl<W: Write + Seek> Seek for BufferedWriter<W> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        self.flush_buffer()?;
        self.inner.seek(pos)
    }
}

/// Opens a file for buffered reading.
pub fn open_buffered(path: &str) -> io::Result<BufferedReader<File>> {
    let file = File::open(path)?;
    Ok(BufferedReader::new(file))
}

/// Creates a file for buffered writing.
pub fn create_buffered(path: &str) -> io::Result<BufferedWriter<File>> {
    let file = File::create(path)?;
    Ok(BufferedWriter::new(file))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_buffered_reader_basic() {
        let data = b"Hello, World!";
        let cursor = Cursor::new(data);
        let mut reader = BufferedReader::new(cursor);

        let mut buf = [0u8; 5];
        assert_eq!(reader.read(&mut buf).unwrap(), 5);
        assert_eq!(&buf, b"Hello");

        assert_eq!(reader.read(&mut buf).unwrap(), 5);
        assert_eq!(&buf, b", Wor");

        let mut buf = [0u8; 10];
        assert_eq!(reader.read(&mut buf).unwrap(), 3);
        assert_eq!(&buf[..3], b"ld!");
    }

    #[test]
    fn test_buffered_reader_with_capacity() {
        let data = b"Test data";
        let cursor = Cursor::new(data);
        let mut reader = BufferedReader::with_capacity(4, cursor);

        let mut buf = [0u8; 9];
        assert_eq!(reader.read(&mut buf).unwrap(), 9);
        assert_eq!(&buf, b"Test data");
    }

    #[test]
    fn test_buffered_reader_bufread() {
        let data = b"Line 1\nLine 2\nLine 3";
        let cursor = Cursor::new(data);
        let mut reader = BufferedReader::new(cursor);

        let mut line = String::new();
        reader.read_line(&mut line).unwrap();
        assert_eq!(line, "Line 1\n");

        line.clear();
        reader.read_line(&mut line).unwrap();
        assert_eq!(line, "Line 2\n");
    }

    #[test]
    fn test_buffered_writer_basic() {
        let mut output = Vec::new();
        {
            let mut writer = BufferedWriter::new(&mut output);
            writer.write_all(b"Hello").unwrap();
            writer.write_all(b", ").unwrap();
            writer.write_all(b"World!").unwrap();
            writer.flush().unwrap();
        }
        assert_eq!(output, b"Hello, World!");
    }

    #[test]
    fn test_buffered_writer_with_capacity() {
        let mut output = Vec::new();
        {
            let mut writer = BufferedWriter::with_capacity(4, &mut output);
            writer.write_all(b"Test").unwrap();
            // Buffer not flushed yet (capacity is 4)
            writer.write_all(b" data").unwrap(); // Triggers flush
                                                 // After writing more than capacity, data is flushed
        }
        assert_eq!(output, b"Test data");
    }

    #[test]
    fn test_buffered_writer_auto_flush() {
        let mut output = Vec::new();
        {
            let mut writer = BufferedWriter::with_capacity(4, &mut output);
            writer.write_all(b"12345").unwrap(); // Larger than buffer
        }
        assert_eq!(output, b"12345");
    }

    #[test]
    fn test_buffered_reader_buffer_len() {
        let data = b"Hello, World!";
        let cursor = Cursor::new(data);
        let mut reader = BufferedReader::with_capacity(8, cursor);

        let mut buf = [0u8; 3];
        reader.read(&mut buf).unwrap();
        assert!(reader.buffer_len() > 0);
    }

    #[test]
    fn test_buffered_writer_buffer_len() {
        let mut output = Vec::new();
        let mut writer = BufferedWriter::with_capacity(16, &mut output);

        writer.write_all(b"Test").unwrap();
        assert_eq!(writer.buffer_len(), 4);

        writer.flush().unwrap();
        assert_eq!(writer.buffer_len(), 0);
    }

    #[test]
    fn test_buffered_reader_into_inner() {
        let data = b"Test";
        let cursor = Cursor::new(data);
        let reader = BufferedReader::new(cursor);
        let inner = reader.into_inner();
        assert_eq!(inner.into_inner(), data);
    }

    #[test]
    fn test_buffered_writer_into_inner() {
        let mut output = Vec::new();
        let mut writer = BufferedWriter::new(&mut output);
        writer.write_all(b"Test").unwrap();
        writer.into_inner().unwrap();
        assert_eq!(output, b"Test");
    }
}
