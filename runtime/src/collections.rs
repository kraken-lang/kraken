//! Collections module providing additional data structures.

#![allow(dead_code)]

use std::collections::{BTreeMap, BTreeSet, BinaryHeap, HashSet, LinkedList, VecDeque};
use std::hash::Hash;

/// HashSet wrapper for Kraken runtime
pub struct HashSetWrapper<T: Eq + Hash> {
    inner: HashSet<T>,
}

impl<T: Eq + Hash> HashSetWrapper<T> {
    /// Create a new empty HashSet
    pub fn new() -> Self {
        Self {
            inner: HashSet::new(),
        }
    }

    /// Create a new HashSet with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: HashSet::with_capacity(capacity),
        }
    }

    /// Insert a value into the set
    pub fn insert(&mut self, value: T) -> bool {
        self.inner.insert(value)
    }

    /// Remove a value from the set
    pub fn remove(&mut self, value: &T) -> bool {
        self.inner.remove(value)
    }

    /// Check if the set contains a value
    pub fn contains(&self, value: &T) -> bool {
        self.inner.contains(value)
    }

    /// Get the number of elements in the set
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the set is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements from the set
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Get the capacity of the set
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// Reserve capacity for at least additional more elements
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }
}

impl<T: Eq + Hash> Default for HashSetWrapper<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// BTreeMap wrapper for Kraken runtime
pub struct BTreeMapWrapper<K: Ord, V> {
    inner: BTreeMap<K, V>,
}

impl<K: Ord, V> BTreeMapWrapper<K, V> {
    /// Create a new empty BTreeMap
    pub fn new() -> Self {
        Self {
            inner: BTreeMap::new(),
        }
    }

    /// Insert a key-value pair into the map
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.inner.insert(key, value)
    }

    /// Get a value from the map
    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }

    /// Get a mutable reference to a value
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.inner.get_mut(key)
    }

    /// Remove a key-value pair from the map
    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.inner.remove(key)
    }

    /// Check if the map contains a key
    pub fn contains_key(&self, key: &K) -> bool {
        self.inner.contains_key(key)
    }

    /// Get the number of elements in the map
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the map is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements from the map
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Get the first key-value pair
    pub fn first_key_value(&self) -> Option<(&K, &V)> {
        self.inner.first_key_value()
    }

    /// Get the last key-value pair
    pub fn last_key_value(&self) -> Option<(&K, &V)> {
        self.inner.last_key_value()
    }
}

impl<K: Ord, V> Default for BTreeMapWrapper<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

/// BTreeSet wrapper for Kraken runtime
pub struct BTreeSetWrapper<T: Ord> {
    inner: BTreeSet<T>,
}

impl<T: Ord> BTreeSetWrapper<T> {
    /// Create a new empty BTreeSet
    pub fn new() -> Self {
        Self {
            inner: BTreeSet::new(),
        }
    }

    /// Insert a value into the set
    pub fn insert(&mut self, value: T) -> bool {
        self.inner.insert(value)
    }

    /// Remove a value from the set
    pub fn remove(&mut self, value: &T) -> bool {
        self.inner.remove(value)
    }

    /// Check if the set contains a value
    pub fn contains(&self, value: &T) -> bool {
        self.inner.contains(value)
    }

    /// Get the number of elements in the set
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the set is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements from the set
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Get the first element
    pub fn first(&self) -> Option<&T> {
        self.inner.first()
    }

    /// Get the last element
    pub fn last(&self) -> Option<&T> {
        self.inner.last()
    }

    /// Remove and return the first element
    pub fn pop_first(&mut self) -> Option<T> {
        self.inner.pop_first()
    }

    /// Remove and return the last element
    pub fn pop_last(&mut self) -> Option<T> {
        self.inner.pop_last()
    }
}

impl<T: Ord> Default for BTreeSetWrapper<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// LinkedList wrapper for Kraken runtime
pub struct LinkedListWrapper<T> {
    inner: LinkedList<T>,
}

impl<T> LinkedListWrapper<T> {
    /// Create a new empty LinkedList
    pub fn new() -> Self {
        Self {
            inner: LinkedList::new(),
        }
    }

    /// Push a value to the front of the list
    pub fn push_front(&mut self, value: T) {
        self.inner.push_front(value);
    }

    /// Push a value to the back of the list
    pub fn push_back(&mut self, value: T) {
        self.inner.push_back(value);
    }

    /// Pop a value from the front of the list
    pub fn pop_front(&mut self) -> Option<T> {
        self.inner.pop_front()
    }

    /// Pop a value from the back of the list
    pub fn pop_back(&mut self) -> Option<T> {
        self.inner.pop_back()
    }

    /// Get a reference to the front element
    pub fn front(&self) -> Option<&T> {
        self.inner.front()
    }

    /// Get a reference to the back element
    pub fn back(&self) -> Option<&T> {
        self.inner.back()
    }

    /// Get the number of elements in the list
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the list is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements from the list
    pub fn clear(&mut self) {
        self.inner.clear();
    }
}

impl<T> Default for LinkedListWrapper<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// VecDeque wrapper for Kraken runtime
pub struct VecDequeWrapper<T> {
    inner: VecDeque<T>,
}

impl<T> VecDequeWrapper<T> {
    /// Create a new empty VecDeque
    pub fn new() -> Self {
        Self {
            inner: VecDeque::new(),
        }
    }

    /// Create a new VecDeque with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: VecDeque::with_capacity(capacity),
        }
    }

    /// Push a value to the front of the deque
    pub fn push_front(&mut self, value: T) {
        self.inner.push_front(value);
    }

    /// Push a value to the back of the deque
    pub fn push_back(&mut self, value: T) {
        self.inner.push_back(value);
    }

    /// Pop a value from the front of the deque
    pub fn pop_front(&mut self) -> Option<T> {
        self.inner.pop_front()
    }

    /// Pop a value from the back of the deque
    pub fn pop_back(&mut self) -> Option<T> {
        self.inner.pop_back()
    }

    /// Get a reference to the front element
    pub fn front(&self) -> Option<&T> {
        self.inner.front()
    }

    /// Get a reference to the back element
    pub fn back(&self) -> Option<&T> {
        self.inner.back()
    }

    /// Get the number of elements in the deque
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the deque is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements from the deque
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Get the capacity of the deque
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// Reserve capacity for at least additional more elements
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    /// Get element at index
    pub fn get(&self, index: usize) -> Option<&T> {
        self.inner.get(index)
    }

    /// Get mutable reference to element at index
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.inner.get_mut(index)
    }
}

impl<T> Default for VecDequeWrapper<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// BinaryHeap wrapper for Kraken runtime
pub struct BinaryHeapWrapper<T: Ord> {
    inner: BinaryHeap<T>,
}

impl<T: Ord> BinaryHeapWrapper<T> {
    /// Create a new empty BinaryHeap
    pub fn new() -> Self {
        Self {
            inner: BinaryHeap::new(),
        }
    }

    /// Create a new BinaryHeap with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: BinaryHeap::with_capacity(capacity),
        }
    }

    /// Push a value into the heap
    pub fn push(&mut self, value: T) {
        self.inner.push(value);
    }

    /// Pop the greatest value from the heap
    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    /// Get a reference to the greatest element
    pub fn peek(&self) -> Option<&T> {
        self.inner.peek()
    }

    /// Get the number of elements in the heap
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the heap is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements from the heap
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Get the capacity of the heap
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// Reserve capacity for at least additional more elements
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }
}

impl<T: Ord> Default for BinaryHeapWrapper<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hashset_new() {
        let set: HashSetWrapper<i32> = HashSetWrapper::new();
        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
    }

    #[test]
    fn test_hashset_insert() {
        let mut set = HashSetWrapper::new();
        assert!(set.insert(1));
        assert!(!set.insert(1));
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn test_hashset_contains() {
        let mut set = HashSetWrapper::new();
        set.insert(42);
        assert!(set.contains(&42));
        assert!(!set.contains(&43));
    }

    #[test]
    fn test_hashset_remove() {
        let mut set = HashSetWrapper::new();
        set.insert(1);
        assert!(set.remove(&1));
        assert!(!set.remove(&1));
        assert!(set.is_empty());
    }

    #[test]
    fn test_hashset_clear() {
        let mut set = HashSetWrapper::new();
        set.insert(1);
        set.insert(2);
        set.clear();
        assert!(set.is_empty());
    }

    #[test]
    fn test_btreemap_new() {
        let map: BTreeMapWrapper<i32, String> = BTreeMapWrapper::new();
        assert!(map.is_empty());
        assert_eq!(map.len(), 0);
    }

    #[test]
    fn test_btreemap_insert() {
        let mut map = BTreeMapWrapper::new();
        assert_eq!(map.insert(1, "one".to_string()), None);
        assert_eq!(map.insert(1, "ONE".to_string()), Some("one".to_string()));
        assert_eq!(map.len(), 1);
    }

    #[test]
    fn test_btreemap_get() {
        let mut map = BTreeMapWrapper::new();
        map.insert(1, "one");
        assert_eq!(map.get(&1), Some(&"one"));
        assert_eq!(map.get(&2), None);
    }

    #[test]
    fn test_btreemap_remove() {
        let mut map = BTreeMapWrapper::new();
        map.insert(1, "one");
        assert_eq!(map.remove(&1), Some("one"));
        assert_eq!(map.remove(&1), None);
    }

    #[test]
    fn test_btreemap_first_last() {
        let mut map = BTreeMapWrapper::new();
        map.insert(1, "one");
        map.insert(3, "three");
        map.insert(2, "two");
        assert_eq!(map.first_key_value(), Some((&1, &"one")));
        assert_eq!(map.last_key_value(), Some((&3, &"three")));
    }

    #[test]
    fn test_btreeset_new() {
        let set: BTreeSetWrapper<i32> = BTreeSetWrapper::new();
        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
    }

    #[test]
    fn test_btreeset_insert() {
        let mut set = BTreeSetWrapper::new();
        assert!(set.insert(1));
        assert!(!set.insert(1));
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn test_btreeset_contains() {
        let mut set = BTreeSetWrapper::new();
        set.insert(42);
        assert!(set.contains(&42));
        assert!(!set.contains(&43));
    }

    #[test]
    fn test_btreeset_first_last() {
        let mut set = BTreeSetWrapper::new();
        set.insert(3);
        set.insert(1);
        set.insert(2);
        assert_eq!(set.first(), Some(&1));
        assert_eq!(set.last(), Some(&3));
    }

    #[test]
    fn test_btreeset_pop() {
        let mut set = BTreeSetWrapper::new();
        set.insert(1);
        set.insert(2);
        set.insert(3);
        assert_eq!(set.pop_first(), Some(1));
        assert_eq!(set.pop_last(), Some(3));
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn test_linkedlist_new() {
        let list: LinkedListWrapper<i32> = LinkedListWrapper::new();
        assert!(list.is_empty());
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn test_linkedlist_push_pop() {
        let mut list = LinkedListWrapper::new();
        list.push_back(1);
        list.push_back(2);
        list.push_front(0);
        assert_eq!(list.len(), 3);
        assert_eq!(list.pop_front(), Some(0));
        assert_eq!(list.pop_back(), Some(2));
        assert_eq!(list.len(), 1);
    }

    #[test]
    fn test_linkedlist_front_back() {
        let mut list = LinkedListWrapper::new();
        list.push_back(1);
        list.push_back(2);
        assert_eq!(list.front(), Some(&1));
        assert_eq!(list.back(), Some(&2));
    }

    #[test]
    fn test_linkedlist_clear() {
        let mut list = LinkedListWrapper::new();
        list.push_back(1);
        list.push_back(2);
        list.clear();
        assert!(list.is_empty());
    }

    #[test]
    fn test_vecdeque_new() {
        let deque: VecDequeWrapper<i32> = VecDequeWrapper::new();
        assert!(deque.is_empty());
        assert_eq!(deque.len(), 0);
    }

    #[test]
    fn test_vecdeque_push_pop() {
        let mut deque = VecDequeWrapper::new();
        deque.push_back(1);
        deque.push_back(2);
        deque.push_front(0);
        assert_eq!(deque.len(), 3);
        assert_eq!(deque.pop_front(), Some(0));
        assert_eq!(deque.pop_back(), Some(2));
        assert_eq!(deque.len(), 1);
    }

    #[test]
    fn test_vecdeque_get() {
        let mut deque = VecDequeWrapper::new();
        deque.push_back(1);
        deque.push_back(2);
        deque.push_back(3);
        assert_eq!(deque.get(0), Some(&1));
        assert_eq!(deque.get(1), Some(&2));
        assert_eq!(deque.get(2), Some(&3));
        assert_eq!(deque.get(3), None);
    }

    #[test]
    fn test_vecdeque_capacity() {
        let deque: VecDequeWrapper<i32> = VecDequeWrapper::with_capacity(10);
        assert!(deque.capacity() >= 10);
    }

    #[test]
    fn test_binaryheap_new() {
        let heap: BinaryHeapWrapper<i32> = BinaryHeapWrapper::new();
        assert!(heap.is_empty());
        assert_eq!(heap.len(), 0);
    }

    #[test]
    fn test_binaryheap_push_pop() {
        let mut heap = BinaryHeapWrapper::new();
        heap.push(3);
        heap.push(1);
        heap.push(2);
        assert_eq!(heap.len(), 3);
        assert_eq!(heap.pop(), Some(3));
        assert_eq!(heap.pop(), Some(2));
        assert_eq!(heap.pop(), Some(1));
        assert!(heap.is_empty());
    }

    #[test]
    fn test_binaryheap_peek() {
        let mut heap = BinaryHeapWrapper::new();
        heap.push(1);
        heap.push(3);
        heap.push(2);
        assert_eq!(heap.peek(), Some(&3));
        assert_eq!(heap.len(), 3);
    }

    #[test]
    fn test_binaryheap_capacity() {
        let heap: BinaryHeapWrapper<i32> = BinaryHeapWrapper::with_capacity(10);
        assert!(heap.capacity() >= 10);
    }
}
