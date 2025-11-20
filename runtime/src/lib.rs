//! Kraken Runtime Library
//! 
//! Provides core runtime functionality for the Kraken programming language,
//! including async runtime abstraction, memory management, and optional
//! garbage collection for development mode.

pub mod async_adapter;
pub mod memory;
pub mod gc;

pub use async_adapter::{AsyncRuntime, TokioRuntime, get_runtime};
pub use memory::{Allocator, MemoryError, MemoryResult};
pub use gc::GarbageCollector;
