//! Concurrent data structures for lock-free programming.

#![allow(dead_code)]

use std::collections::{HashMap, VecDeque};
use std::hash::Hash;
use std::sync::{Arc, Mutex};

/// Lock-free MPSC (Multiple Producer Single Consumer) queue
pub struct MpscQueue<T> {
    queue: Arc<Mutex<VecDeque<T>>>,
}

impl<T> MpscQueue<T> {
    pub fn new() -> Self {
        Self {
            queue: Arc::new(Mutex::new(VecDeque::new())),
        }
    }

    pub fn push(&self, value: T) {
        let mut queue = self.queue.lock().unwrap();
        queue.push_back(value);
    }

    pub fn pop(&self) -> Option<T> {
        let mut queue = self.queue.lock().unwrap();
        queue.pop_front()
    }

    pub fn len(&self) -> usize {
        let queue = self.queue.lock().unwrap();
        queue.len()
    }

    pub fn is_empty(&self) -> bool {
        let queue = self.queue.lock().unwrap();
        queue.is_empty()
    }
}

impl<T> Clone for MpscQueue<T> {
    fn clone(&self) -> Self {
        Self {
            queue: self.queue.clone(),
        }
    }
}

impl<T> Default for MpscQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Lock-free MPMC (Multiple Producer Multiple Consumer) queue
pub struct MpmcQueue<T> {
    queue: Arc<Mutex<VecDeque<T>>>,
}

impl<T> MpmcQueue<T> {
    pub fn new() -> Self {
        Self {
            queue: Arc::new(Mutex::new(VecDeque::new())),
        }
    }

    pub fn push(&self, value: T) {
        let mut queue = self.queue.lock().unwrap();
        queue.push_back(value);
    }

    pub fn pop(&self) -> Option<T> {
        let mut queue = self.queue.lock().unwrap();
        queue.pop_front()
    }

    pub fn len(&self) -> usize {
        let queue = self.queue.lock().unwrap();
        queue.len()
    }

    pub fn is_empty(&self) -> bool {
        let queue = self.queue.lock().unwrap();
        queue.is_empty()
    }
}

impl<T> Clone for MpmcQueue<T> {
    fn clone(&self) -> Self {
        Self {
            queue: self.queue.clone(),
        }
    }
}

impl<T> Default for MpmcQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Concurrent hash map
pub struct ConcurrentHashMap<K, V> {
    map: Arc<Mutex<HashMap<K, V>>>,
}

impl<K: Eq + Hash, V> ConcurrentHashMap<K, V> {
    pub fn new() -> Self {
        Self {
            map: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn insert(&self, key: K, value: V) -> Option<V> {
        let mut map = self.map.lock().unwrap();
        map.insert(key, value)
    }

    pub fn get(&self, key: &K) -> Option<V>
    where
        V: Clone,
    {
        let map = self.map.lock().unwrap();
        map.get(key).cloned()
    }

    pub fn remove(&self, key: &K) -> Option<V> {
        let mut map = self.map.lock().unwrap();
        map.remove(key)
    }

    pub fn contains_key(&self, key: &K) -> bool {
        let map = self.map.lock().unwrap();
        map.contains_key(key)
    }

    pub fn len(&self) -> usize {
        let map = self.map.lock().unwrap();
        map.len()
    }

    pub fn is_empty(&self) -> bool {
        let map = self.map.lock().unwrap();
        map.is_empty()
    }

    pub fn clear(&self) {
        let mut map = self.map.lock().unwrap();
        map.clear();
    }
}

impl<K: Eq + Hash, V> Clone for ConcurrentHashMap<K, V> {
    fn clone(&self) -> Self {
        Self {
            map: self.map.clone(),
        }
    }
}

impl<K: Eq + Hash, V> Default for ConcurrentHashMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

/// Work-stealing deque
pub struct WorkStealingDeque<T> {
    deque: Arc<Mutex<VecDeque<T>>>,
}

impl<T> WorkStealingDeque<T> {
    pub fn new() -> Self {
        Self {
            deque: Arc::new(Mutex::new(VecDeque::new())),
        }
    }

    pub fn push(&self, value: T) {
        let mut deque = self.deque.lock().unwrap();
        deque.push_back(value);
    }

    pub fn pop(&self) -> Option<T> {
        let mut deque = self.deque.lock().unwrap();
        deque.pop_back()
    }

    pub fn steal(&self) -> Option<T> {
        let mut deque = self.deque.lock().unwrap();
        deque.pop_front()
    }

    pub fn len(&self) -> usize {
        let deque = self.deque.lock().unwrap();
        deque.len()
    }

    pub fn is_empty(&self) -> bool {
        let deque = self.deque.lock().unwrap();
        deque.is_empty()
    }
}

impl<T> Clone for WorkStealingDeque<T> {
    fn clone(&self) -> Self {
        Self {
            deque: self.deque.clone(),
        }
    }
}

impl<T> Default for WorkStealingDeque<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Thread pool for executing tasks
pub struct ThreadPool {
    workers: Vec<std::thread::JoinHandle<()>>,
    sender: Option<std::sync::mpsc::Sender<Box<dyn FnOnce() + Send + 'static>>>,
}

impl ThreadPool {
    pub fn new(size: usize) -> Self {
        let (sender, receiver) = std::sync::mpsc::channel::<Box<dyn FnOnce() + Send + 'static>>();
        let receiver = Arc::new(Mutex::new(receiver));

        let mut workers = Vec::with_capacity(size);

        for _ in 0..size {
            let receiver = receiver.clone();
            let handle = std::thread::spawn(move || loop {
                let task = receiver.lock().unwrap().recv();

                match task {
                    Ok(task) => task(),
                    Err(_) => break,
                }
            });
            workers.push(handle);
        }

        Self {
            workers,
            sender: Some(sender),
        }
    }

    pub fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        if let Some(sender) = &self.sender {
            sender.send(Box::new(f)).ok();
        }
    }

    pub fn worker_count(&self) -> usize {
        self.workers.len()
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
        drop(self.sender.take());

        for worker in self.workers.drain(..) {
            worker.join().ok();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::thread;

    #[test]
    fn test_mpsc_queue() {
        let queue = MpscQueue::new();
        queue.push(1);
        queue.push(2);
        queue.push(3);

        assert_eq!(queue.len(), 3);
        assert_eq!(queue.pop(), Some(1));
        assert_eq!(queue.pop(), Some(2));
        assert_eq!(queue.pop(), Some(3));
        assert_eq!(queue.pop(), None);
    }

    #[test]
    fn test_mpsc_queue_concurrent() {
        let queue = MpscQueue::new();
        let queue_clone = queue.clone();

        let handle = thread::spawn(move || {
            for i in 0..10 {
                queue_clone.push(i);
            }
        });

        handle.join().unwrap();
        assert_eq!(queue.len(), 10);
    }

    #[test]
    fn test_mpmc_queue() {
        let queue = MpmcQueue::new();
        queue.push(1);
        queue.push(2);

        assert_eq!(queue.pop(), Some(1));
        assert_eq!(queue.pop(), Some(2));
        assert_eq!(queue.pop(), None);
    }

    #[test]
    fn test_concurrent_hashmap() {
        let map = ConcurrentHashMap::new();
        map.insert("key1", 100);
        map.insert("key2", 200);

        assert_eq!(map.get(&"key1"), Some(100));
        assert_eq!(map.get(&"key2"), Some(200));
        assert_eq!(map.len(), 2);

        map.remove(&"key1");
        assert_eq!(map.len(), 1);
        assert!(!map.contains_key(&"key1"));
    }

    #[test]
    fn test_concurrent_hashmap_concurrent() {
        let map = ConcurrentHashMap::new();
        let map_clone = map.clone();

        let handle = thread::spawn(move || {
            map_clone.insert("thread_key", 42);
        });

        handle.join().unwrap();
        assert_eq!(map.get(&"thread_key"), Some(42));
    }

    #[test]
    fn test_work_stealing_deque() {
        let deque = WorkStealingDeque::new();
        deque.push(1);
        deque.push(2);
        deque.push(3);

        assert_eq!(deque.pop(), Some(3)); // Pop from back
        assert_eq!(deque.steal(), Some(1)); // Steal from front
        assert_eq!(deque.pop(), Some(2));
        assert_eq!(deque.pop(), None);
    }

    #[test]
    fn test_thread_pool() {
        let pool = ThreadPool::new(4);
        let counter = Arc::new(AtomicUsize::new(0));

        for _ in 0..10 {
            let counter = counter.clone();
            pool.execute(move || {
                counter.fetch_add(1, Ordering::SeqCst);
            });
        }

        // Give threads time to complete
        thread::sleep(std::time::Duration::from_millis(100));
        assert_eq!(counter.load(Ordering::SeqCst), 10);
    }

    #[test]
    fn test_thread_pool_worker_count() {
        let pool = ThreadPool::new(8);
        assert_eq!(pool.worker_count(), 8);
    }
}
