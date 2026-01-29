//! Collection extension methods for improved ergonomics.
//!
//! Provides convenience methods for common collection operations.

use std::collections::HashMap;

/// Extension trait for Vec with convenience methods.
pub trait VecExt<T> {
    /// Extend the vector with elements from an iterator.
    fn extend_from<I: IntoIterator<Item = T>>(&mut self, iter: I);

    /// Retain only elements that satisfy the predicate.
    fn retain_if<F: FnMut(&T) -> bool>(&mut self, f: F);

    /// Remove and return the first element matching the predicate.
    fn remove_first<F: FnMut(&T) -> bool>(&mut self, f: F) -> Option<T>;

    /// Check if all elements satisfy the predicate.
    fn all<F: FnMut(&T) -> bool>(&self, f: F) -> bool;

    /// Check if any element satisfies the predicate.
    fn any<F: FnMut(&T) -> bool>(&self, f: F) -> bool;

    /// Partition into two vectors based on predicate.
    fn partition<F: FnMut(&T) -> bool>(&self, f: F) -> (Vec<T>, Vec<T>)
    where
        T: Clone;

    /// Get a slice of elements.
    fn slice(&self, start: usize, end: usize) -> &[T];

    /// Chunk the vector into fixed-size pieces.
    fn chunks(&self, size: usize) -> Vec<&[T]>;
}

impl<T> VecExt<T> for Vec<T> {
    fn extend_from<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.extend(iter);
    }

    fn retain_if<F: FnMut(&T) -> bool>(&mut self, f: F) {
        self.retain(f);
    }

    fn remove_first<F: FnMut(&T) -> bool>(&mut self, f: F) -> Option<T> {
        self.iter().position(f).map(|i| self.remove(i))
    }

    fn all<F: FnMut(&T) -> bool>(&self, f: F) -> bool {
        self.iter().all(f)
    }

    fn any<F: FnMut(&T) -> bool>(&self, f: F) -> bool {
        self.iter().any(f)
    }

    fn partition<F: FnMut(&T) -> bool>(&self, mut f: F) -> (Vec<T>, Vec<T>)
    where
        T: Clone,
    {
        let mut true_vec = Vec::new();
        let mut false_vec = Vec::new();
        for item in self {
            if f(item) {
                true_vec.push(item.clone());
            } else {
                false_vec.push(item.clone());
            }
        }
        (true_vec, false_vec)
    }

    fn slice(&self, start: usize, end: usize) -> &[T] {
        &self[start..end.min(self.len())]
    }

    fn chunks(&self, size: usize) -> Vec<&[T]> {
        self.as_slice().chunks(size).collect()
    }
}

/// Extension trait for HashMap with convenience methods.
pub trait HashMapExt<K, V> {
    /// Get a value or insert a default.
    fn get_or_insert(&mut self, key: K, default: V) -> &mut V;

    /// Get a value or insert the result of a function.
    fn get_or_insert_with<F: FnOnce() -> V>(&mut self, key: K, f: F) -> &mut V;

    /// Update a value if the key exists.
    fn update<F: FnOnce(&mut V)>(&mut self, key: &K, f: F) -> bool;

    /// Merge another map into this one.
    fn merge(&mut self, other: HashMap<K, V>);

    /// Filter the map by predicate.
    fn filter<F: FnMut(&K, &V) -> bool>(&self, f: F) -> HashMap<K, V>
    where
        K: Clone,
        V: Clone;
}

impl<K: Eq + std::hash::Hash, V> HashMapExt<K, V> for HashMap<K, V> {
    fn get_or_insert(&mut self, key: K, default: V) -> &mut V {
        self.entry(key).or_insert(default)
    }

    fn get_or_insert_with<F: FnOnce() -> V>(&mut self, key: K, f: F) -> &mut V {
        self.entry(key).or_insert_with(f)
    }

    fn update<F: FnOnce(&mut V)>(&mut self, key: &K, f: F) -> bool {
        if let Some(value) = self.get_mut(key) {
            f(value);
            true
        } else {
            false
        }
    }

    fn merge(&mut self, other: HashMap<K, V>) {
        self.extend(other);
    }

    fn filter<F: FnMut(&K, &V) -> bool>(&self, mut f: F) -> HashMap<K, V>
    where
        K: Clone,
        V: Clone,
    {
        self.iter()
            .filter(|(k, v)| f(k, v))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_extend_from() {
        let mut v = vec![1, 2, 3];
        v.extend_from(vec![4, 5, 6]);
        assert_eq!(v, vec![1, 2, 3, 4, 5, 6]);
    }

    #[test]
    fn test_vec_retain_if() {
        let mut v = vec![1, 2, 3, 4, 5];
        v.retain_if(|x| x % 2 == 0);
        assert_eq!(v, vec![2, 4]);
    }

    #[test]
    fn test_vec_remove_first() {
        let mut v = vec![1, 2, 3, 4, 5];
        let removed = v.remove_first(|x| *x > 3);
        assert_eq!(removed, Some(4));
        assert_eq!(v, vec![1, 2, 3, 5]);
    }

    #[test]
    fn test_vec_all_any() {
        let v = vec![2, 4, 6, 8];
        assert!(v.all(|x| x % 2 == 0));
        assert!(!v.all(|x| *x > 5));

        assert!(v.any(|x| *x > 5));
        assert!(!v.any(|x| *x > 10));
    }

    #[test]
    fn test_vec_partition() {
        let v = vec![1, 2, 3, 4, 5, 6];
        let (evens, odds) = v.partition(|x| x % 2 == 0);
        assert_eq!(evens, vec![2, 4, 6]);
        assert_eq!(odds, vec![1, 3, 5]);
    }

    #[test]
    fn test_vec_slice() {
        let v = vec![1, 2, 3, 4, 5];
        assert_eq!(v.slice(1, 4), &[2, 3, 4]);
    }

    #[test]
    fn test_hashmap_get_or_insert() {
        let mut map = HashMap::new();
        map.insert("a", 1);

        let val = map.get_or_insert("a", 10);
        assert_eq!(*val, 1);

        let val = map.get_or_insert("b", 20);
        assert_eq!(*val, 20);
    }

    #[test]
    fn test_hashmap_update() {
        let mut map = HashMap::new();
        map.insert("a", 1);

        assert!(map.update(&"a", |v| *v += 10));
        assert_eq!(map.get(&"a"), Some(&11));

        assert!(!map.update(&"b", |v| *v += 10));
    }

    #[test]
    fn test_hashmap_merge() {
        let mut map1 = HashMap::new();
        map1.insert("a", 1);
        map1.insert("b", 2);

        let mut map2 = HashMap::new();
        map2.insert("c", 3);
        map2.insert("d", 4);

        map1.merge(map2);
        assert_eq!(map1.len(), 4);
    }

    #[test]
    fn test_hashmap_filter() {
        let mut map = HashMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("c", 3);
        map.insert("d", 4);

        let filtered = map.filter(|_, v| *v % 2 == 0);
        assert_eq!(filtered.len(), 2);
        assert_eq!(filtered.get(&"b"), Some(&2));
        assert_eq!(filtered.get(&"d"), Some(&4));
    }
}
