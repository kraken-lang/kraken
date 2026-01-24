//! Kraken Runtime Library
//!
//! Provides core runtime functionality for the Kraken programming language,
//! including async runtime abstraction, memory management, and optional
//! garbage collection for development mode.

pub mod async_adapter;
pub mod async_io;
pub mod async_primitives;
pub mod async_runtime;
pub mod ffi;
pub mod gc;
pub mod memory;
pub mod smart_pointers;

pub use async_adapter::{get_runtime, AsyncRuntime, TokioRuntime};
pub use async_io::{
    AsyncFile, AsyncStream, AsyncTcpListener, AsyncTcpStream, AsyncTimer, AsyncUdpSocket,
};
pub use async_primitives::{
    async_channel, oneshot, AsyncBarrier, AsyncMutex, AsyncReceiver, AsyncSemaphore, AsyncSender,
    OneshotReceiver, OneshotSender,
};
pub use async_runtime::{Executor, Scheduler, Task, TaskId};
pub use gc::GarbageCollector;
pub use memory::{Allocator, MemoryError, MemoryResult};
pub use smart_pointers::{ArcRuntime, BoxRuntime, RcRuntime};
