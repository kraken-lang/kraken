//! Kraken Runtime Library
//!
//! Provides core runtime functionality for the Kraken programming language,
//! including async runtime abstraction, memory management, and optional
//! garbage collection for development mode.

pub mod async_adapter;
pub mod ffi;
pub mod gc;
pub mod memory;

pub use async_adapter::{get_runtime, AsyncRuntime, TokioRuntime};
pub use gc::GarbageCollector;
pub use memory::{Allocator, MemoryError, MemoryResult};
