//! Async runtime implementation with executor and scheduler.

#![allow(dead_code)]

use std::collections::VecDeque;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll, Wake, Waker};

/// Task identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TaskId(usize);

/// Task for the async runtime
pub struct Task {
    id: TaskId,
    future: Pin<Box<dyn Future<Output = ()> + Send>>,
}

impl Task {
    pub fn new(id: TaskId, future: Pin<Box<dyn Future<Output = ()> + Send>>) -> Self {
        Self { id, future }
    }

    pub fn id(&self) -> TaskId {
        self.id
    }

    pub fn poll(&mut self, context: &mut Context) -> Poll<()> {
        self.future.as_mut().poll(context)
    }
}

/// Simple waker implementation
struct SimpleWaker {
    task_id: TaskId,
    queue: Arc<Mutex<VecDeque<TaskId>>>,
}

impl Wake for SimpleWaker {
    fn wake(self: Arc<Self>) {
        self.wake_by_ref();
    }

    fn wake_by_ref(self: &Arc<Self>) {
        let mut queue = self.queue.lock().unwrap();
        queue.push_back(self.task_id);
    }
}

/// Single-threaded executor
pub struct Executor {
    tasks: Vec<Task>,
    ready_queue: Arc<Mutex<VecDeque<TaskId>>>,
    next_task_id: usize,
}

impl Executor {
    pub fn new() -> Self {
        Self {
            tasks: Vec::new(),
            ready_queue: Arc::new(Mutex::new(VecDeque::new())),
            next_task_id: 0,
        }
    }

    /// Spawn a new task
    pub fn spawn(&mut self, future: Pin<Box<dyn Future<Output = ()> + Send>>) -> TaskId {
        let task_id = TaskId(self.next_task_id);
        self.next_task_id += 1;

        let task = Task::new(task_id, future);
        self.tasks.push(task);

        // Add to ready queue
        self.ready_queue.lock().unwrap().push_back(task_id);

        task_id
    }

    /// Run the executor until all tasks complete
    pub fn run(&mut self) {
        while !self.tasks.is_empty() {
            // Get next ready task
            let task_id = {
                let mut queue = self.ready_queue.lock().unwrap();
                if let Some(id) = queue.pop_front() {
                    id
                } else {
                    break; // No more ready tasks
                }
            };

            // Find and poll the task
            if let Some(task) = self.tasks.iter_mut().find(|t| t.id() == task_id) {
                let waker = Arc::new(SimpleWaker {
                    task_id,
                    queue: self.ready_queue.clone(),
                });
                let waker = Waker::from(waker);
                let mut context = Context::from_waker(&waker);

                match task.poll(&mut context) {
                    Poll::Ready(()) => {
                        // Task completed, remove it
                        self.tasks.retain(|t| t.id() != task_id);
                    }
                    Poll::Pending => {
                        // Task not ready, will be woken later
                    }
                }
            }
        }
    }

    /// Get the number of active tasks
    pub fn task_count(&self) -> usize {
        self.tasks.len()
    }
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

/// Task scheduler with work-stealing (simplified implementation)
pub struct Scheduler {
    executors: Vec<Executor>,
}

impl Scheduler {
    pub fn new(num_threads: usize) -> Self {
        let mut executors = Vec::with_capacity(num_threads);
        for _ in 0..num_threads {
            executors.push(Executor::new());
        }
        Self { executors }
    }

    /// Spawn a task on the least loaded executor
    pub fn spawn(&mut self, future: Pin<Box<dyn Future<Output = ()> + Send>>) -> TaskId {
        // Find executor with fewest tasks
        let executor = self
            .executors
            .iter_mut()
            .min_by_key(|e| e.task_count())
            .expect("No executors available");

        executor.spawn(future)
    }

    /// Run all executors
    pub fn run(&mut self) {
        for executor in &mut self.executors {
            executor.run();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, Ordering};

    struct TestFuture {
        completed: Arc<AtomicBool>,
    }

    impl Future for TestFuture {
        type Output = ();

        fn poll(self: Pin<&mut Self>, _cx: &mut Context) -> Poll<Self::Output> {
            self.completed.store(true, Ordering::SeqCst);
            Poll::Ready(())
        }
    }

    #[test]
    fn test_executor_creation() {
        let executor = Executor::new();
        assert_eq!(executor.task_count(), 0);
    }

    #[test]
    fn test_spawn_task() {
        let mut executor = Executor::new();
        let completed = Arc::new(AtomicBool::new(false));
        let future = Box::pin(TestFuture {
            completed: completed.clone(),
        });

        executor.spawn(future);
        assert_eq!(executor.task_count(), 1);
    }

    #[test]
    fn test_run_executor() {
        let mut executor = Executor::new();
        let completed = Arc::new(AtomicBool::new(false));
        let future = Box::pin(TestFuture {
            completed: completed.clone(),
        });

        executor.spawn(future);
        executor.run();

        assert!(completed.load(Ordering::SeqCst));
        assert_eq!(executor.task_count(), 0);
    }

    #[test]
    fn test_scheduler_creation() {
        let scheduler = Scheduler::new(4);
        assert_eq!(scheduler.executors.len(), 4);
    }

    #[test]
    fn test_scheduler_spawn() {
        let mut scheduler = Scheduler::new(2);
        let completed = Arc::new(AtomicBool::new(false));
        let future = Box::pin(TestFuture {
            completed: completed.clone(),
        });

        scheduler.spawn(future);
        scheduler.run();

        assert!(completed.load(Ordering::SeqCst));
    }
}
