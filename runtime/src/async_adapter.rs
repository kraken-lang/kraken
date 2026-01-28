use std::future::Future;

use std::pin::Pin;

/// Async runtime abstraction trait for swappable runtime implementations.
///
/// This trait provides a unified interface for different async runtimes,
/// allowing the Kraken compiler to switch between Tokio (default) and
/// Cycle (high-performance alternative) via feature flags.
///
/// Note: Uses boxed futures to maintain object safety.
pub trait AsyncRuntime: Send + Sync {
    /// Spawn a future onto the runtime's executor.
    ///
    /// # Arguments
    /// * `future` - The async task to execute (boxed for object safety)
    fn spawn(&self, future: Pin<Box<dyn Future<Output = ()> + Send + 'static>>);

    /// Block the current thread until the future completes.
    ///
    /// # Arguments
    /// * `future` - The async task to execute and wait for (boxed for object safety)
    ///
    /// # Returns
    /// The output of the completed future
    fn block_on(&self, future: Pin<Box<dyn Future<Output = ()> + Send>>) -> ();
}

/// Tokio runtime implementation (default).
///
/// Provides async execution using the Tokio runtime, which is the
/// default choice for the Kraken compiler due to its maturity and
/// ecosystem support.
#[cfg(feature = "tokio")]
pub struct TokioRuntime {
    handle: tokio::runtime::Handle,
}

#[cfg(feature = "tokio")]
impl TokioRuntime {
    /// Create a new TokioRuntime using the current runtime handle.
    ///
    /// # Panics
    /// Panics if called outside of a Tokio runtime context.
    pub fn new() -> Self {
        Self {
            handle: tokio::runtime::Handle::current(),
        }
    }

    /// Create a TokioRuntime from an existing handle.
    pub fn from_handle(handle: tokio::runtime::Handle) -> Self {
        Self { handle }
    }
}

#[cfg(feature = "tokio")]
impl Default for TokioRuntime {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "tokio")]
impl AsyncRuntime for TokioRuntime {
    fn spawn(&self, future: Pin<Box<dyn Future<Output = ()> + Send + 'static>>) {
        self.handle.spawn(future);
    }

    fn block_on(&self, future: Pin<Box<dyn Future<Output = ()> + Send>>) {
        self.handle.block_on(future);
    }
}

/// Get the default async runtime
///
/// Returns TokioRuntime as the default async runtime.
///
/// # Returns
/// A boxed trait object implementing AsyncRuntime
pub fn get_runtime() -> Box<dyn AsyncRuntime> {
    Box::new(TokioRuntime::new())
}
