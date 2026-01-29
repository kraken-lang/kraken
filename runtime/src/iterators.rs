/// Iterator trait implementations and utilities for Kraken runtime.
///
/// This module provides iterator support for collections, filling a gap identified
/// in the 0.8.40 standard library analysis.
use std::marker::PhantomData;

// Re-export standard library iterator traits to avoid conflicts
pub use std::iter::{FromIterator, IntoIterator, Iterator};

/// Map iterator adapter.
pub struct Map<I, F> {
    iter: I,
    f: F,
}

impl<I, F> Map<I, F> {
    pub fn new(iter: I, f: F) -> Self {
        Self { iter, f }
    }
}

impl<B, I: Iterator, F> Iterator for Map<I, F>
where
    F: FnMut(I::Item) -> B,
{
    type Item = B;

    fn next(&mut self) -> Option<B> {
        self.iter.next().map(&mut self.f)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

/// Filter iterator adapter.
pub struct Filter<I, P> {
    iter: I,
    predicate: P,
}

impl<I, P> Filter<I, P> {
    pub fn new(iter: I, predicate: P) -> Self {
        Self { iter, predicate }
    }
}

impl<I: Iterator, P> Iterator for Filter<I, P>
where
    P: FnMut(&I::Item) -> bool,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        self.iter.by_ref().find(|x| (self.predicate)(x))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (_, upper) = self.iter.size_hint();
        (0, upper)
    }
}

/// Range iterator for numeric ranges.
pub struct Range<T> {
    start: T,
    end: T,
}

impl<T> Range<T> {
    pub fn new(start: T, end: T) -> Self {
        Self { start, end }
    }
}

impl Iterator for Range<i64> {
    type Item = i64;

    fn next(&mut self) -> Option<i64> {
        if self.start < self.end {
            let val = self.start;
            self.start += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = (self.end - self.start).max(0) as usize;
        (len, Some(len))
    }
}

impl Iterator for Range<usize> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.start < self.end {
            let val = self.start;
            self.start += 1;
            Some(val)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.end.saturating_sub(self.start);
        (len, Some(len))
    }
}

/// Empty iterator that yields no elements.
pub struct Empty<T> {
    _marker: PhantomData<T>,
}

impl<T> Empty<T> {
    pub fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

impl<T> Default for Empty<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Iterator for Empty<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(0))
    }
}

/// Once iterator that yields exactly one element.
pub struct Once<T> {
    value: Option<T>,
}

impl<T> Once<T> {
    pub fn new(value: T) -> Self {
        Self { value: Some(value) }
    }
}

impl<T> Iterator for Once<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.value.take()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = if self.value.is_some() { 1 } else { 0 };
        (len, Some(len))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_range_iterator() {
        let mut iter = Range::new(0i64, 5i64);
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_range_size_hint() {
        let iter = Range::new(0i64, 10i64);
        assert_eq!(iter.size_hint(), (10, Some(10)));
    }

    #[test]
    fn test_map() {
        let iter = Range::new(0i64, 5i64);
        let doubled: Vec<i64> = iter.map(|x| x * 2).collect();
        assert_eq!(doubled, vec![0, 2, 4, 6, 8]);
    }

    #[test]
    fn test_filter() {
        let iter = Range::new(0i64, 10i64);
        let evens: Vec<i64> = iter.filter(|x| x % 2 == 0).collect();
        assert_eq!(evens, vec![0, 2, 4, 6, 8]);
    }

    #[test]
    fn test_fold() {
        let iter = Range::new(1i64, 6i64);
        let sum: i64 = iter.sum();
        assert_eq!(sum, 15);
    }

    #[test]
    fn test_any() {
        let mut iter = Range::new(0i64, 5i64);
        assert!(iter.any(|x| x == 3));

        let mut iter = Range::new(0i64, 5i64);
        assert!(!iter.any(|x| x == 10));
    }

    #[test]
    fn test_all() {
        let mut iter = Range::new(0i64, 5i64);
        assert!(iter.all(|x| x < 10));

        let mut iter = Range::new(0i64, 5i64);
        assert!(!iter.all(|x| x < 3));
    }

    #[test]
    fn test_find() {
        let mut iter = Range::new(0i64, 10i64);
        assert_eq!(iter.find(|x| *x > 5), Some(6));

        let mut iter = Range::new(0i64, 10i64);
        assert_eq!(iter.find(|x| *x > 20), None);
    }

    #[test]
    fn test_count() {
        let iter = Range::new(0i64, 10i64);
        assert_eq!(iter.count(), 10);
    }

    #[test]
    fn test_last() {
        let iter = Range::new(0i64, 5i64);
        assert_eq!(iter.last(), Some(4));

        let iter = Range::new(0i64, 0i64);
        assert_eq!(iter.last(), None);
    }

    #[test]
    fn test_nth() {
        let mut iter = Range::new(0i64, 10i64);
        assert_eq!(iter.nth(5), Some(5));
        assert_eq!(iter.nth(2), Some(8));
        assert_eq!(iter.nth(10), None);
    }

    #[test]
    fn test_empty_iterator() {
        let mut iter = Empty::<i64>::new();
        assert_eq!(iter.next(), None);
        assert_eq!(iter.size_hint(), (0, Some(0)));
    }

    #[test]
    fn test_once_iterator() {
        let mut iter = Once::new(42);
        assert_eq!(iter.next(), Some(42));
        assert_eq!(iter.next(), None);
    }
}

// Note: FromIterator and IntoIterator for Vec are already implemented in std
