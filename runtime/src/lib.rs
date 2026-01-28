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
pub mod collections;
pub mod concurrent;
pub mod connection_pool;
pub mod crypto;
pub mod datetime;
pub mod enhanced_serialization;
pub mod ffi;
pub mod file_io;
pub mod gc;
pub mod generic_collections;
pub mod http;
pub mod math;
pub mod memory;
pub mod networking;
pub mod path_types;
pub mod platform;
pub mod random;
pub mod registry;
pub mod serialization;
pub mod signals;
pub mod smart_pointers;
pub mod string_format;
pub mod string_types;
pub mod sync;
pub mod text_processing;
pub mod thread_safety;
pub mod threading;
pub mod time_types;
pub mod utilities;
pub mod win32;
pub mod windows_service;

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
pub use collections::{
    BTreeMapWrapper, BTreeSetWrapper, BinaryHeapWrapper, HashSetWrapper, LinkedListWrapper,
    VecDequeWrapper,
};
pub use concurrent::{ConcurrentHashMap, MpmcQueue, MpscQueue, ThreadPool, WorkStealingDeque};
pub use connection_pool::{ConnectionPool, PoolStats, PooledHttpClient};
pub use crypto::{
    constant_time_compare, Aes256, Base64, ChaCha20Poly1305, Ed25519, Hmac, Pbkdf2, Sha256, Sha512,
};
pub use datetime::{Date, DateTime, Time, TimeZone, UtcOffset};
pub use enhanced_serialization::{
    CborSerializer, IniSerializer, MessagePackSerializer, TomlSerializer, YamlSerializer,
};
pub use file_io::{DirectoryEntry, DirectoryTraversal, FileUtils, MemoryMappedFile};
pub use gc::GarbageCollector;
pub use generic_collections::{Map, SliceUtils, Vec};
pub use http::{HttpClient, HttpMethod, HttpRequest, HttpResponse, HttpServer};
pub use math::{Mat2, Mat3, Mat4, Quaternion, Statistics, Trigonometry, Vec2, Vec3, Vec4};
pub use memory::{Allocator, MemoryError, MemoryResult};
pub use networking::{IpAddress, TcpListenerSocket, TcpSocket, UdpSocketWrapper};
pub use path_types::{OsStringUtils, PathUtils as PathTypesUtils};
pub use platform::{execute, execute_shell, PathUtils, Platform, Process, ProcessBuilder};
pub use random::{
    thread_rng, ChaCha20Rng, Distributions, PcgRng, RandomNumberGenerator, SeedableRng, XorshiftRng,
};
pub use registry::{Registry, RegistryHive};
pub use serialization::{BinarySerializer, JsonSerializer, JsonValue};
pub use signals::{ignore_signal, raise_signal, register_ctrl_c_handler, Signal, SignalHandler};
pub use smart_pointers::{ArcRuntime, BoxRuntime, RcRuntime};
pub use string_format::{Alignment, StringFormatter, StringInterpolator};
pub use string_types::{StringBuilder, StringUtils};
pub use sync::{Barrier, ConditionVariable, Mutex, RwLock, Semaphore};
pub use text_processing::{
    AdvancedStringUtils, CaseMapper, RegexWrapper, UnicodeNormalizer, UnicodeSegmenter,
};
pub use thread_safety::{DeadlockDetector, PerformanceMetrics, PerformanceMonitor};
pub use threading::{
    current_thread_id, current_thread_name, sleep, spawn, yield_now, Thread, ThreadBuilder,
    ThreadLocal,
};
pub use time_types::{CStringUtils, DurationUtils, InstantUtils, SystemTimeUtils};
pub use utilities::{CliParser, Compression, EnvVars, LogLevel, Logger, Uuid};
pub use win32::{MessageBoxResult, MessageBoxType, Win32};
pub use windows_service::{ServiceStartType, ServiceState, WindowsService};
