//! Iterator support for HashMap, HashSet, BTreeMap, and BTreeSet collections.

#![allow(dead_code)]

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::hash::Hash;

/// Drain iterator for HashMap
pub struct HashMapDrain<'a, K, V> {
    inner: std::collections::hash_map::Drain<'a, K, V>,
}

impl<'a, K, V> HashMapDrain<'a, K, V> {
    pub fn new(map: &'a mut HashMap<K, V>) -> Self {
        Self { inner: map.drain() }
    }
}

impl<'a, K, V> Iterator for HashMapDrain<'a, K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

/// Drain iterator for HashSet
pub struct HashSetDrain<'a, T> {
    inner: std::collections::hash_set::Drain<'a, T>,
}

impl<'a, T> HashSetDrain<'a, T> {
    pub fn new(set: &'a mut HashSet<T>) -> Self {
        Self { inner: set.drain() }
    }
}

impl<'a, T> Iterator for HashSetDrain<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

// Note: BTreeMap and BTreeSet drain iterators are available via std::collections
// Users can call .drain() directly on these collections

/// Iterator utilities for HashMap
pub struct HashMapIterators;

impl HashMapIterators {
    /// Create an iterator over keys
    pub fn keys<K, V>(map: &HashMap<K, V>) -> impl Iterator<Item = &K> {
        map.keys()
    }

    /// Create an iterator over values
    pub fn values<K, V>(map: &HashMap<K, V>) -> impl Iterator<Item = &V> {
        map.values()
    }

    /// Create an iterator over key-value pairs
    pub fn iter<K, V>(map: &HashMap<K, V>) -> impl Iterator<Item = (&K, &V)> {
        map.iter()
    }

    /// Create a mutable iterator over values
    pub fn values_mut<K, V>(map: &mut HashMap<K, V>) -> impl Iterator<Item = &mut V> {
        map.values_mut()
    }

    /// Create a mutable iterator over key-value pairs
    pub fn iter_mut<K, V>(map: &mut HashMap<K, V>) -> impl Iterator<Item = (&K, &mut V)> {
        map.iter_mut()
    }

    /// Drain all elements from the map
    pub fn drain<K, V>(map: &mut HashMap<K, V>) -> HashMapDrain<K, V> {
        HashMapDrain::new(map)
    }
}

/// Iterator utilities for HashSet
pub struct HashSetIterators;

impl HashSetIterators {
    /// Create an iterator over elements
    pub fn iter<T>(set: &HashSet<T>) -> impl Iterator<Item = &T> {
        set.iter()
    }

    /// Drain all elements from the set
    pub fn drain<T>(set: &mut HashSet<T>) -> HashSetDrain<T> {
        HashSetDrain::new(set)
    }

    /// Create an iterator over the union of two sets
    pub fn union<'a, T: Eq + Hash>(
        set1: &'a HashSet<T>,
        set2: &'a HashSet<T>,
    ) -> impl Iterator<Item = &'a T> {
        set1.union(set2)
    }

    /// Create an iterator over the intersection of two sets
    pub fn intersection<'a, T: Eq + Hash>(
        set1: &'a HashSet<T>,
        set2: &'a HashSet<T>,
    ) -> impl Iterator<Item = &'a T> {
        set1.intersection(set2)
    }

    /// Create an iterator over the difference of two sets
    pub fn difference<'a, T: Eq + Hash>(
        set1: &'a HashSet<T>,
        set2: &'a HashSet<T>,
    ) -> impl Iterator<Item = &'a T> {
        set1.difference(set2)
    }

    /// Create an iterator over the symmetric difference of two sets
    pub fn symmetric_difference<'a, T: Eq + Hash>(
        set1: &'a HashSet<T>,
        set2: &'a HashSet<T>,
    ) -> impl Iterator<Item = &'a T> {
        set1.symmetric_difference(set2)
    }
}

/// Iterator utilities for BTreeMap
pub struct BTreeMapIterators;

impl BTreeMapIterators {
    /// Create an iterator over keys
    pub fn keys<K, V>(map: &BTreeMap<K, V>) -> impl Iterator<Item = &K> {
        map.keys()
    }

    /// Create an iterator over values
    pub fn values<K, V>(map: &BTreeMap<K, V>) -> impl Iterator<Item = &V> {
        map.values()
    }

    /// Create an iterator over key-value pairs
    pub fn iter<K, V>(map: &BTreeMap<K, V>) -> impl Iterator<Item = (&K, &V)> {
        map.iter()
    }

    /// Create a mutable iterator over values
    pub fn values_mut<K, V>(map: &mut BTreeMap<K, V>) -> impl Iterator<Item = &mut V> {
        map.values_mut()
    }

    /// Create a mutable iterator over key-value pairs
    pub fn iter_mut<K, V>(map: &mut BTreeMap<K, V>) -> impl Iterator<Item = (&K, &mut V)> {
        map.iter_mut()
    }

    /// Create an iterator over a range of keys
    pub fn range<K: Ord, V, R>(map: &BTreeMap<K, V>, range: R) -> impl Iterator<Item = (&K, &V)>
    where
        R: std::ops::RangeBounds<K>,
    {
        map.range(range)
    }
}

/// Iterator utilities for BTreeSet
pub struct BTreeSetIterators;

impl BTreeSetIterators {
    /// Create an iterator over elements
    pub fn iter<T>(set: &BTreeSet<T>) -> impl Iterator<Item = &T> {
        set.iter()
    }

    /// Create an iterator over a range of elements
    pub fn range<T: Ord, R>(set: &BTreeSet<T>, range: R) -> impl Iterator<Item = &T>
    where
        R: std::ops::RangeBounds<T>,
    {
        set.range(range)
    }

    /// Create an iterator over the union of two sets
    pub fn union<'a, T: Ord>(
        set1: &'a BTreeSet<T>,
        set2: &'a BTreeSet<T>,
    ) -> impl Iterator<Item = &'a T> {
        set1.union(set2)
    }

    /// Create an iterator over the intersection of two sets
    pub fn intersection<'a, T: Ord>(
        set1: &'a BTreeSet<T>,
        set2: &'a BTreeSet<T>,
    ) -> impl Iterator<Item = &'a T> {
        set1.intersection(set2)
    }

    /// Create an iterator over the difference of two sets
    pub fn difference<'a, T: Ord>(
        set1: &'a BTreeSet<T>,
        set2: &'a BTreeSet<T>,
    ) -> impl Iterator<Item = &'a T> {
        set1.difference(set2)
    }

    /// Create an iterator over the symmetric difference of two sets
    pub fn symmetric_difference<'a, T: Ord>(
        set1: &'a BTreeSet<T>,
        set2: &'a BTreeSet<T>,
    ) -> impl Iterator<Item = &'a T> {
        set1.symmetric_difference(set2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hashmap_keys_iterator() {
        let mut map = HashMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("c", 3);

        let keys: Vec<_> = HashMapIterators::keys(&map).collect();
        assert_eq!(keys.len(), 3);
    }

    #[test]
    fn test_hashmap_values_iterator() {
        let mut map = HashMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("c", 3);

        let values: Vec<_> = HashMapIterators::values(&map).collect();
        assert_eq!(values.len(), 3);
    }

    #[test]
    fn test_hashmap_drain() {
        let mut map = HashMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("c", 3);

        let drained: Vec<_> = HashMapIterators::drain(&mut map).collect();
        assert_eq!(drained.len(), 3);
        assert!(map.is_empty());
    }

    #[test]
    fn test_hashset_iterator() {
        let mut set = HashSet::new();
        set.insert(1);
        set.insert(2);
        set.insert(3);

        let items: Vec<_> = HashSetIterators::iter(&set).collect();
        assert_eq!(items.len(), 3);
    }

    #[test]
    fn test_hashset_drain() {
        let mut set = HashSet::new();
        set.insert(1);
        set.insert(2);
        set.insert(3);

        let drained: Vec<_> = HashSetIterators::drain(&mut set).collect();
        assert_eq!(drained.len(), 3);
        assert!(set.is_empty());
    }

    #[test]
    fn test_hashset_union() {
        let mut set1 = HashSet::new();
        set1.insert(1);
        set1.insert(2);

        let mut set2 = HashSet::new();
        set2.insert(2);
        set2.insert(3);

        let union: Vec<_> = HashSetIterators::union(&set1, &set2).collect();
        assert_eq!(union.len(), 3);
    }

    #[test]
    fn test_hashset_intersection() {
        let mut set1 = HashSet::new();
        set1.insert(1);
        set1.insert(2);

        let mut set2 = HashSet::new();
        set2.insert(2);
        set2.insert(3);

        let intersection: Vec<_> = HashSetIterators::intersection(&set1, &set2).collect();
        assert_eq!(intersection.len(), 1);
    }

    #[test]
    fn test_btreemap_keys_iterator() {
        let mut map = BTreeMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("c", 3);

        let keys: Vec<_> = BTreeMapIterators::keys(&map).collect();
        assert_eq!(keys, vec![&"a", &"b", &"c"]);
    }

    #[test]
    fn test_btreemap_values_iterator() {
        let mut map = BTreeMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("c", 3);

        let values: Vec<_> = BTreeMapIterators::values(&map).collect();
        assert_eq!(values, vec![&1, &2, &3]);
    }

    #[test]
    fn test_btreemap_range() {
        let mut map = BTreeMap::new();
        map.insert(1, "a");
        map.insert(2, "b");
        map.insert(3, "c");
        map.insert(4, "d");

        let range: Vec<_> = BTreeMapIterators::range(&map, 2..=3).collect();
        assert_eq!(range.len(), 2);
    }

    #[test]
    fn test_btreeset_iterator() {
        let mut set = BTreeSet::new();
        set.insert(3);
        set.insert(1);
        set.insert(2);

        let items: Vec<_> = BTreeSetIterators::iter(&set).collect();
        assert_eq!(items, vec![&1, &2, &3]);
    }

    #[test]
    fn test_btreeset_range() {
        let mut set = BTreeSet::new();
        set.insert(1);
        set.insert(2);
        set.insert(3);
        set.insert(4);
        set.insert(5);

        let range: Vec<_> = BTreeSetIterators::range(&set, 2..=4).collect();
        assert_eq!(range, vec![&2, &3, &4]);
    }

    #[test]
    fn test_btreeset_union() {
        let mut set1 = BTreeSet::new();
        set1.insert(1);
        set1.insert(2);

        let mut set2 = BTreeSet::new();
        set2.insert(2);
        set2.insert(3);

        let union: Vec<_> = BTreeSetIterators::union(&set1, &set2).collect();
        assert_eq!(union, vec![&1, &2, &3]);
    }

    #[test]
    fn test_btreeset_intersection() {
        let mut set1 = BTreeSet::new();
        set1.insert(1);
        set1.insert(2);

        let mut set2 = BTreeSet::new();
        set2.insert(2);
        set2.insert(3);

        let intersection: Vec<_> = BTreeSetIterators::intersection(&set1, &set2).collect();
        assert_eq!(intersection, vec![&2]);
    }
}
