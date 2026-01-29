//! Parallel compilation support for bootstrap compiler.
//!
//! Provides infrastructure for compiling multiple modules in parallel.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

/// Compilation job for parallel execution.
#[derive(Debug, Clone)]
pub struct CompilationJob {
    pub path: PathBuf,
    pub source: String,
    pub dependencies: Vec<PathBuf>,
}

impl CompilationJob {
    /// Create a new compilation job.
    pub fn new(path: PathBuf, source: String, dependencies: Vec<PathBuf>) -> Self {
        Self {
            path,
            source,
            dependencies,
        }
    }
}

/// Result of a compilation job.
#[derive(Debug, Clone)]
pub struct CompilationResult {
    pub path: PathBuf,
    pub success: bool,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

impl CompilationResult {
    /// Create a successful result.
    pub fn success(path: PathBuf) -> Self {
        Self {
            path,
            success: true,
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    /// Create a failed result.
    pub fn failure(path: PathBuf, errors: Vec<String>) -> Self {
        Self {
            path,
            success: false,
            errors,
            warnings: Vec::new(),
        }
    }

    /// Add a warning.
    pub fn with_warning(mut self, warning: String) -> Self {
        self.warnings.push(warning);
        self
    }
}

/// Parallel compiler for compiling multiple modules.
pub struct ParallelCompiler {
    jobs: Vec<CompilationJob>,
    results: Arc<Mutex<HashMap<PathBuf, CompilationResult>>>,
    max_threads: usize,
}

impl ParallelCompiler {
    /// Create a new parallel compiler.
    pub fn new(max_threads: usize) -> Self {
        Self {
            jobs: Vec::new(),
            results: Arc::new(Mutex::new(HashMap::new())),
            max_threads,
        }
    }

    /// Add a compilation job.
    pub fn add_job(&mut self, job: CompilationJob) {
        self.jobs.push(job);
    }

    /// Get the number of jobs.
    pub fn job_count(&self) -> usize {
        self.jobs.len()
    }

    /// Compile all jobs in parallel.
    pub fn compile_all(&mut self) -> Vec<CompilationResult> {
        let chunk_size = self.jobs.len().div_ceil(self.max_threads);
        let mut handles = Vec::new();

        for chunk in self.jobs.chunks(chunk_size) {
            let jobs = chunk.to_vec();
            let results = Arc::clone(&self.results);

            let handle = std::thread::spawn(move || {
                for job in jobs {
                    let result = Self::compile_job(&job);
                    let mut results = results.lock().unwrap();
                    results.insert(job.path.clone(), result);
                }
            });

            handles.push(handle);
        }

        for handle in handles {
            handle.join().unwrap();
        }

        let results = self.results.lock().unwrap();
        results.values().cloned().collect()
    }

    /// Compile a single job.
    fn compile_job(job: &CompilationJob) -> CompilationResult {
        // Simplified compilation - in real implementation would call actual compiler
        if job.source.is_empty() {
            CompilationResult::failure(job.path.clone(), vec!["Empty source file".to_string()])
        } else {
            CompilationResult::success(job.path.clone())
        }
    }

    /// Get compilation results.
    pub fn results(&self) -> HashMap<PathBuf, CompilationResult> {
        self.results.lock().unwrap().clone()
    }

    /// Check if all compilations succeeded.
    pub fn all_succeeded(&self) -> bool {
        let results = self.results.lock().unwrap();
        results.values().all(|r| r.success)
    }

    /// Get failed compilations.
    pub fn failed_compilations(&self) -> Vec<PathBuf> {
        let results = self.results.lock().unwrap();
        results
            .iter()
            .filter(|(_, r)| !r.success)
            .map(|(path, _)| path.clone())
            .collect()
    }
}

/// Thread pool for parallel compilation.
pub struct CompilerThreadPool {
    thread_count: usize,
}

impl CompilerThreadPool {
    /// Create a new thread pool.
    pub fn new(thread_count: usize) -> Self {
        Self { thread_count }
    }

    /// Get the number of threads.
    pub fn thread_count(&self) -> usize {
        self.thread_count
    }

    /// Get the optimal thread count for the system.
    pub fn optimal_thread_count() -> usize {
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4)
    }
}

impl Default for CompilerThreadPool {
    fn default() -> Self {
        Self::new(Self::optimal_thread_count())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compilation_job() {
        let job = CompilationJob::new(PathBuf::from("test.kr"), "fn main() {}".to_string(), vec![]);
        assert_eq!(job.path, PathBuf::from("test.kr"));
        assert_eq!(job.dependencies.len(), 0);
    }

    #[test]
    fn test_compilation_result_success() {
        let result = CompilationResult::success(PathBuf::from("test.kr"));
        assert!(result.success);
        assert_eq!(result.errors.len(), 0);
    }

    #[test]
    fn test_compilation_result_failure() {
        let result =
            CompilationResult::failure(PathBuf::from("test.kr"), vec!["error 1".to_string()]);
        assert!(!result.success);
        assert_eq!(result.errors.len(), 1);
    }

    #[test]
    fn test_parallel_compiler() {
        let mut compiler = ParallelCompiler::new(2);
        assert_eq!(compiler.job_count(), 0);

        compiler.add_job(CompilationJob::new(
            PathBuf::from("test1.kr"),
            "fn main() {}".to_string(),
            vec![],
        ));
        assert_eq!(compiler.job_count(), 1);
    }

    #[test]
    fn test_parallel_compiler_compile() {
        let mut compiler = ParallelCompiler::new(2);
        compiler.add_job(CompilationJob::new(
            PathBuf::from("test1.kr"),
            "fn main() {}".to_string(),
            vec![],
        ));
        compiler.add_job(CompilationJob::new(
            PathBuf::from("test2.kr"),
            "fn foo() {}".to_string(),
            vec![],
        ));

        let results = compiler.compile_all();
        assert_eq!(results.len(), 2);
        assert!(compiler.all_succeeded());
    }

    #[test]
    fn test_parallel_compiler_failure() {
        let mut compiler = ParallelCompiler::new(2);
        compiler.add_job(CompilationJob::new(
            PathBuf::from("test1.kr"),
            "".to_string(),
            vec![],
        ));

        let results = compiler.compile_all();
        assert_eq!(results.len(), 1);
        assert!(!compiler.all_succeeded());

        let failed = compiler.failed_compilations();
        assert_eq!(failed.len(), 1);
    }

    #[test]
    fn test_thread_pool() {
        let pool = CompilerThreadPool::new(4);
        assert_eq!(pool.thread_count(), 4);

        let optimal = CompilerThreadPool::optimal_thread_count();
        assert!(optimal > 0);
    }

    #[test]
    fn test_thread_pool_default() {
        let pool = CompilerThreadPool::default();
        assert!(pool.thread_count() > 0);
    }
}
