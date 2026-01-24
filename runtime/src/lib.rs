//! Kraken Runtime Library
//!
//! Provides core runtime functionality for the Kraken programming language,
//! including async runtime abstraction, memory management, and optional
//! garbage collection for development mode.

pub mod async_adapter;
pub mod async_io;
pub mod async_primitives;
pub mod async_runtime;
pub mod atomic;
pub mod concurrent;
pub mod ffi;
pub mod gc;
pub mod memory;
pub mod smart_pointers;
pub mod sync;
pub mod thread_safety;
pub mod threading;

pub use async_adapter::{get_runtime, AsyncRuntime, TokioRuntime};
pub use async_io::{
    AsyncFile, AsyncStream, AsyncTcpListener, AsyncTcpStream, AsyncTimer, AsyncUdpSocket,
};
pub use async_primitives::{
    async_channel, oneshot, AsyncBarrier, AsyncMutex, AsyncReceiver, AsyncSemaphore, AsyncSender,
    OneshotReceiver, OneshotSender,
};
pub use async_runtime::{Executor, Scheduler, Task, TaskId};
pub use atomic::{
    AtomicBoolWrapper, AtomicI32Wrapper, AtomicI64Wrapper, AtomicIsizeWrapper, AtomicU32Wrapper,
    AtomicU64Wrapper, AtomicUsizeWrapper, Ordering,
};
pub use concurrent::{ConcurrentHashMap, MpmcQueue, MpscQueue, ThreadPool, WorkStealingDeque};
pub use gc::GarbageCollector;
pub use memory::{Allocator, MemoryError, MemoryResult};
pub use smart_pointers::{ArcRuntime, BoxRuntime, RcRuntime};
pub use sync::{Barrier, ConditionVariable, Mutex, RwLock, Semaphore};
pub use thread_safety::{DeadlockDetector, PerformanceMetrics, PerformanceMonitor};
pub use threading::{
    current_thread_id, current_thread_name, sleep, spawn, yield_now, Thread, ThreadBuilder,
    ThreadLocal,
};
