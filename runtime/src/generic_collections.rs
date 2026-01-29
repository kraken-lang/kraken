//! Generic collections module providing Vec<T>, HashMap<K,V>, and slice utilities.

#![allow(dead_code)]

use std::collections::HashMap;
use std::hash::Hash;

/// Generic Vec<T> wrapper for Kraken runtime
pub struct Vec<T> {
    inner: std::vec::Vec<T>,
}

impl<T> Vec<T> {
    /// Create a new empty Vec
    pub fn new() -> Self {
        Self {
            inner: std::vec::Vec::new(),
        }
    }

    /// Create a new Vec with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: std::vec::Vec::with_capacity(capacity),
        }
    }

    /// Push a value to the end of the vector
    pub fn push(&mut self, value: T) {
        self.inner.push(value);
    }

    /// Pop a value from the end of the vector
    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    /// Get a reference to an element at index
    pub fn get(&self, index: usize) -> Option<&T> {
        self.inner.get(index)
    }

    /// Get a mutable reference to an element at index
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.inner.get_mut(index)
    }

    /// Get the number of elements in the vector
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the vector is empty
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Clear all elements from the vector
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Get the capacity of the vector
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// Reserve capacity for at least additional more elements
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    /// Insert an element at position index
    pub fn insert(&mut self, index: usize, element: T) {
        self.inner.insert(index, element);
    }

    /// Remove and return the element at position index
    pub fn remove(&mut self, index: usize) -> T {
        self.inner.remove(index)
    }

    /// Retain only the elements specified by the predicate
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.inner.retain(f);
    }

    /// Get the first element
    pub fn first(&self) -> Option<&T> {
        self.inner.first()
    }

    /// Get the last element
    pub fn last(&self) -> Option<&T> {
        self.inner.last()
    }

    /// Reverse the vector in place
    pub fn reverse(&mut self) {
        self.inner.reverse();
    }

    /// Sort the vector
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.inner.sort();
    }

    /// Sort the vector by a key function
    pub fn sort_by_key<K, F>(&mut self, f: F)
    where
        F: FnMut(&T) -> K,
        K: Ord,
    {
        self.inner.sort_by_key(f);
    }

    /// Extend the vector with an iterator
    pub fn extend_from_slice(&mut self, other: &[T])
    where
        T: Clone,
    {
        self.inner.extend_from_slice(other);
    }

    /// Truncate the vector to len elements
    pub fn truncate(&mut self, len: usize) {
        self.inner.truncate(len);
    }

    /// Resize the vector to new_len, filling with value
    pub fn resize(&mut self, new_len: usize, value: T)
    where
        T: Clone,
    {
        self.inner.resize(new_len, value);
    }

    /// Check if the vector contains an element
    pub fn contains(&self, x: &T) -> bool
    where
        T: PartialEq,
    {
        self.inner.contains(x)
    }

    /// Append all elements from other to self
    pub fn append(&mut self, other: &mut Vec<T>) {
        self.inner.append(&mut other.inner);
    }
}

impl<T> Default for Vec<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> Clone for Vec<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

/// Generic HashMap<K, V> wrapper for Kraken runtime
pub struct Map<K: Eq + Hash, V> {
    inner: HashMap<K, V>,
}

impl<K: Eq + Hash, V> Map<K, V> {
    /// Create a new empty HashMap
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    /// Create a new HashMap with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: HashMap::with_capacity(capacity),
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

    /// Get the capacity of the map
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// Reserve capacity for at least additional more elements
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    /// Retain only the elements specified by the predicate
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&K, &mut V) -> bool,
    {
        self.inner.retain(f);
    }
}

impl<K: Eq + Hash, V> Default for Map<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash + Clone, V: Clone> Clone for Map<K, V> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

/// Generic slice utilities
pub struct SliceUtils;

impl SliceUtils {
    /// Get the length of a slice
    pub fn len<T>(slice: &[T]) -> usize {
        slice.len()
    }

    /// Check if a slice is empty
    pub fn is_empty<T>(slice: &[T]) -> bool {
        slice.is_empty()
    }

    /// Get the first element of a slice
    pub fn first<T>(slice: &[T]) -> Option<&T> {
        slice.first()
    }

    /// Get the last element of a slice
    pub fn last<T>(slice: &[T]) -> Option<&T> {
        slice.last()
    }

    /// Get a subslice
    pub fn get_slice<T>(slice: &[T], start: usize, end: usize) -> &[T] {
        &slice[start..end]
    }

    /// Check if a slice contains an element
    pub fn contains<T: PartialEq>(slice: &[T], x: &T) -> bool {
        slice.contains(x)
    }

    /// Reverse a slice in place
    pub fn reverse<T>(slice: &mut [T]) {
        slice.reverse();
    }

    /// Sort a slice
    pub fn sort<T: Ord>(slice: &mut [T]) {
        slice.sort();
    }

    /// Binary search in a sorted slice
    pub fn binary_search<T: Ord>(slice: &[T], x: &T) -> Result<usize, usize> {
        slice.binary_search(x)
    }

    /// Split a slice at an index
    pub fn split_at<T>(slice: &[T], mid: usize) -> (&[T], &[T]) {
        slice.split_at(mid)
    }

    /// Get a chunk of elements
    pub fn chunks<T>(slice: &[T], chunk_size: usize) -> std::slice::Chunks<'_, T> {
        slice.chunks(chunk_size)
    }

    /// Fill a slice with a value
    pub fn fill<T: Clone>(slice: &mut [T], value: T) {
        slice.fill(value);
    }

    /// Swap two elements in a slice
    pub fn swap<T>(slice: &mut [T], a: usize, b: usize) {
        slice.swap(a, b);
    }

    /// Rotate left by mid positions
    pub fn rotate_left<T>(slice: &mut [T], mid: usize) {
        slice.rotate_left(mid);
    }

    /// Rotate right by k positions
    pub fn rotate_right<T>(slice: &mut [T], k: usize) {
        slice.rotate_right(k);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_new() {
        let vec: Vec<i32> = Vec::new();
        assert!(vec.is_empty());
        assert_eq!(vec.len(), 0);
    }

    #[test]
    fn test_vec_push_pop() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);
        assert_eq!(vec.len(), 3);
        assert_eq!(vec.pop(), Some(3));
        assert_eq!(vec.pop(), Some(2));
        assert_eq!(vec.len(), 1);
    }

    #[test]
    fn test_vec_get() {
        let mut vec = Vec::new();
        vec.push(10);
        vec.push(20);
        vec.push(30);
        assert_eq!(vec.get(0), Some(&10));
        assert_eq!(vec.get(1), Some(&20));
        assert_eq!(vec.get(2), Some(&30));
        assert_eq!(vec.get(3), None);
    }

    #[test]
    fn test_vec_insert_remove() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(3);
        vec.insert(1, 2);
        assert_eq!(vec.get(0), Some(&1));
        assert_eq!(vec.get(1), Some(&2));
        assert_eq!(vec.get(2), Some(&3));
        assert_eq!(vec.remove(1), 2);
        assert_eq!(vec.len(), 2);
    }

    #[test]
    fn test_vec_first_last() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);
        assert_eq!(vec.first(), Some(&1));
        assert_eq!(vec.last(), Some(&3));
    }

    #[test]
    fn test_vec_reverse() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);
        vec.reverse();
        assert_eq!(vec.get(0), Some(&3));
        assert_eq!(vec.get(1), Some(&2));
        assert_eq!(vec.get(2), Some(&1));
    }

    #[test]
    fn test_vec_sort() {
        let mut vec = Vec::new();
        vec.push(3);
        vec.push(1);
        vec.push(2);
        vec.sort();
        assert_eq!(vec.get(0), Some(&1));
        assert_eq!(vec.get(1), Some(&2));
        assert_eq!(vec.get(2), Some(&3));
    }

    #[test]
    fn test_vec_contains() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);
        assert!(vec.contains(&2));
        assert!(!vec.contains(&4));
    }

    #[test]
    fn test_vec_capacity() {
        let vec: Vec<i32> = Vec::with_capacity(10);
        assert!(vec.capacity() >= 10);
    }

    #[test]
    fn test_vec_clear() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.clear();
        assert!(vec.is_empty());
    }

    #[test]
    fn test_vec_truncate() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);
        vec.truncate(2);
        assert_eq!(vec.len(), 2);
    }

    #[test]
    fn test_map_new() {
        let map: Map<i32, String> = Map::new();
        assert!(map.is_empty());
        assert_eq!(map.len(), 0);
    }

    #[test]
    fn test_map_insert_get() {
        let mut map = Map::new();
        map.insert(1, "one");
        map.insert(2, "two");
        assert_eq!(map.get(&1), Some(&"one"));
        assert_eq!(map.get(&2), Some(&"two"));
        assert_eq!(map.get(&3), None);
    }

    #[test]
    fn test_map_remove() {
        let mut map = Map::new();
        map.insert(1, "one");
        assert_eq!(map.remove(&1), Some("one"));
        assert_eq!(map.remove(&1), None);
    }

    #[test]
    fn test_map_contains_key() {
        let mut map = Map::new();
        map.insert(1, "one");
        assert!(map.contains_key(&1));
        assert!(!map.contains_key(&2));
    }

    #[test]
    fn test_map_clear() {
        let mut map = Map::new();
        map.insert(1, "one");
        map.insert(2, "two");
        map.clear();
        assert!(map.is_empty());
    }

    #[test]
    fn test_map_capacity() {
        let map: Map<i32, String> = Map::with_capacity(10);
        assert!(map.capacity() >= 10);
    }

    #[test]
    fn test_slice_len() {
        let arr = [1, 2, 3, 4, 5];
        assert_eq!(SliceUtils::len(&arr), 5);
    }

    #[test]
    fn test_slice_is_empty() {
        let arr: [i32; 0] = [];
        assert!(SliceUtils::is_empty(&arr));
        let arr2 = [1, 2, 3];
        assert!(!SliceUtils::is_empty(&arr2));
    }

    #[test]
    fn test_slice_first_last() {
        let arr = [1, 2, 3, 4, 5];
        assert_eq!(SliceUtils::first(&arr), Some(&1));
        assert_eq!(SliceUtils::last(&arr), Some(&5));
    }

    #[test]
    fn test_slice_contains() {
        let arr = [1, 2, 3, 4, 5];
        assert!(SliceUtils::contains(&arr, &3));
        assert!(!SliceUtils::contains(&arr, &6));
    }

    #[test]
    fn test_slice_reverse() {
        let mut arr = [1, 2, 3, 4, 5];
        SliceUtils::reverse(&mut arr);
        assert_eq!(arr, [5, 4, 3, 2, 1]);
    }

    #[test]
    fn test_slice_sort() {
        let mut arr = [3, 1, 4, 1, 5];
        SliceUtils::sort(&mut arr);
        assert_eq!(arr, [1, 1, 3, 4, 5]);
    }

    #[test]
    fn test_slice_binary_search() {
        let arr = [1, 2, 3, 4, 5];
        assert_eq!(SliceUtils::binary_search(&arr, &3), Ok(2));
        assert!(SliceUtils::binary_search(&arr, &6).is_err());
    }

    #[test]
    fn test_slice_split_at() {
        let arr = [1, 2, 3, 4, 5];
        let (left, right) = SliceUtils::split_at(&arr, 2);
        assert_eq!(left, &[1, 2]);
        assert_eq!(right, &[3, 4, 5]);
    }

    #[test]
    fn test_slice_swap() {
        let mut arr = [1, 2, 3, 4, 5];
        SliceUtils::swap(&mut arr, 0, 4);
        assert_eq!(arr, [5, 2, 3, 4, 1]);
    }

    #[test]
    fn test_slice_rotate() {
        let mut arr = [1, 2, 3, 4, 5];
        SliceUtils::rotate_left(&mut arr, 2);
        assert_eq!(arr, [3, 4, 5, 1, 2]);

        let mut arr2 = [1, 2, 3, 4, 5];
        SliceUtils::rotate_right(&mut arr2, 2);
        assert_eq!(arr2, [4, 5, 1, 2, 3]);
    }

    // Edge case tests for Vec
    #[test]
    fn test_vec_empty_operations() {
        let mut vec: Vec<i32> = Vec::new();
        assert_eq!(vec.pop(), None);
        assert_eq!(vec.get(0), None);
        assert_eq!(vec.first(), None);
        assert_eq!(vec.last(), None);
        assert!(vec.is_empty());
    }

    #[test]
    fn test_vec_single_element() {
        let mut vec = Vec::new();
        vec.push(42);
        assert_eq!(vec.len(), 1);
        assert_eq!(vec.first(), Some(&42));
        assert_eq!(vec.last(), Some(&42));
        assert_eq!(vec.pop(), Some(42));
        assert!(vec.is_empty());
    }

    #[test]
    fn test_vec_large_capacity() {
        let vec: Vec<i32> = Vec::with_capacity(1000);
        assert!(vec.capacity() >= 1000);
        assert_eq!(vec.len(), 0);
    }

    #[test]
    fn test_vec_push_pop_sequence() {
        let mut vec = Vec::new();
        for i in 0..100 {
            vec.push(i);
        }
        assert_eq!(vec.len(), 100);
        for i in (0..100).rev() {
            assert_eq!(vec.pop(), Some(i));
        }
        assert!(vec.is_empty());
    }

    #[test]
    fn test_vec_truncate_to_zero() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);
        vec.truncate(0);
        assert!(vec.is_empty());
    }

    #[test]
    fn test_vec_insert_remove_boundary() {
        let mut vec = Vec::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);
        vec.insert(0, 0);
        assert_eq!(vec.get(0), Some(&0));
        vec.remove(0);
        assert_eq!(vec.get(0), Some(&1));
    }

    // Edge case tests for Map
    #[test]
    fn test_map_empty_operations() {
        let map: Map<i32, String> = Map::new();
        assert_eq!(map.get(&1), None);
        assert!(!map.contains_key(&1));
        assert!(map.is_empty());
    }

    #[test]
    fn test_map_single_entry() {
        let mut map = Map::new();
        map.insert(1, "one");
        assert_eq!(map.len(), 1);
        assert_eq!(map.get(&1), Some(&"one"));
        map.remove(&1);
        assert!(map.is_empty());
    }

    #[test]
    fn test_map_overwrite_value() {
        let mut map = Map::new();
        map.insert(1, "one");
        map.insert(1, "ONE");
        assert_eq!(map.len(), 1);
        assert_eq!(map.get(&1), Some(&"ONE"));
    }

    #[test]
    fn test_map_large_capacity() {
        let map: Map<i32, String> = Map::with_capacity(1000);
        assert!(map.capacity() >= 1000);
        assert_eq!(map.len(), 0);
    }

    #[test]
    fn test_map_many_insertions() {
        let mut map = Map::new();
        for i in 0..100 {
            map.insert(i, i.to_string());
        }
        assert_eq!(map.len(), 100);
        for i in 0..100 {
            assert_eq!(map.get(&i), Some(&i.to_string()));
        }
    }

    // Edge case tests for Slice
    #[test]
    fn test_slice_empty_operations() {
        let arr: [i32; 0] = [];
        assert_eq!(SliceUtils::len(&arr), 0);
        assert!(SliceUtils::is_empty(&arr));
        assert_eq!(SliceUtils::first(&arr), None);
        assert_eq!(SliceUtils::last(&arr), None);
    }

    #[test]
    fn test_slice_single_element() {
        let arr = [42];
        assert_eq!(SliceUtils::len(&arr), 1);
        assert_eq!(SliceUtils::first(&arr), Some(&42));
        assert_eq!(SliceUtils::last(&arr), Some(&42));
    }

    #[test]
    fn test_slice_binary_search_empty() {
        let arr: [i32; 0] = [];
        assert!(SliceUtils::binary_search(&arr, &1).is_err());
    }

    #[test]
    fn test_slice_split_at_boundaries() {
        let arr = [1, 2, 3];
        let (left, right) = SliceUtils::split_at(&arr, 0);
        assert_eq!(left, &[]);
        assert_eq!(right, &[1, 2, 3]);
        
        let (left, right) = SliceUtils::split_at(&arr, 3);
        assert_eq!(left, &[1, 2, 3]);
        assert_eq!(right, &[]);
    }
}
