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

/// Zip iterator that combines two iterators into pairs
pub struct Zip<A, B> {
    a: A,
    b: B,
}

impl<A, B> Zip<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A: Iterator, B: Iterator> Iterator for Zip<A, B> {
    type Item = (A::Item, B::Item);

    fn next(&mut self) -> Option<Self::Item> {
        match (self.a.next(), self.b.next()) {
            (Some(a), Some(b)) => Some((a, b)),
            _ => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (a_lower, a_upper) = self.a.size_hint();
        let (b_lower, b_upper) = self.b.size_hint();

        let lower = a_lower.min(b_lower);
        let upper = match (a_upper, b_upper) {
            (Some(a), Some(b)) => Some(a.min(b)),
            _ => None,
        };

        (lower, upper)
    }
}

/// Chain iterator that chains two iterators together
pub struct Chain<A, B> {
    a: Option<A>,
    b: B,
}

impl<A, B> Chain<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a: Some(a), b }
    }
}

impl<A: Iterator, B: Iterator<Item = A::Item>> Iterator for Chain<A, B> {
    type Item = A::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ref mut a) = self.a {
            if let Some(item) = a.next() {
                return Some(item);
            }
            self.a = None;
        }
        self.b.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (a_lower, a_upper) = self.a.as_ref().map_or((0, Some(0)), |a| a.size_hint());
        let (b_lower, b_upper) = self.b.size_hint();

        let lower = a_lower.saturating_add(b_lower);
        let upper = match (a_upper, b_upper) {
            (Some(a), Some(b)) => a.checked_add(b),
            _ => None,
        };

        (lower, upper)
    }
}

/// Take iterator that yields at most n elements
pub struct Take<I> {
    iter: I,
    n: usize,
}

impl<I> Take<I> {
    pub fn new(iter: I, n: usize) -> Self {
        Self { iter, n }
    }
}

impl<I: Iterator> Iterator for Take<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        if self.n > 0 {
            self.n -= 1;
            self.iter.next()
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.iter.size_hint();
        let lower = lower.min(self.n);
        let upper = upper.map(|u| u.min(self.n));
        (lower, upper)
    }
}

/// Skip iterator that skips the first n elements
pub struct Skip<I> {
    iter: I,
    n: usize,
}

impl<I> Skip<I> {
    pub fn new(iter: I, n: usize) -> Self {
        Self { iter, n }
    }
}

impl<I: Iterator> Iterator for Skip<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        while self.n > 0 {
            self.n -= 1;
            self.iter.next()?;
        }
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.iter.size_hint();
        let lower = lower.saturating_sub(self.n);
        let upper = upper.map(|u| u.saturating_sub(self.n));
        (lower, upper)
    }
}

/// Enumerate iterator that yields (index, item) pairs
pub struct Enumerate<I> {
    iter: I,
    count: usize,
}

impl<I> Enumerate<I> {
    pub fn new(iter: I) -> Self {
        Self { iter, count: 0 }
    }
}

impl<I: Iterator> Iterator for Enumerate<I> {
    type Item = (usize, I::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.iter.next()?;
        let count = self.count;
        self.count += 1;
        Some((count, item))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

/// Flatten iterator that flattens nested iterators
pub struct Flatten<I>
where
    I: Iterator,
    I::Item: IntoIterator,
{
    outer: I,
    inner: Option<<I::Item as IntoIterator>::IntoIter>,
}

impl<I> Flatten<I>
where
    I: Iterator,
    I::Item: IntoIterator,
{
    pub fn new(iter: I) -> Self {
        Self {
            outer: iter,
            inner: None,
        }
    }
}

impl<I> Iterator for Flatten<I>
where
    I: Iterator,
    I::Item: IntoIterator,
{
    type Item = <I::Item as IntoIterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(ref mut inner) = self.inner {
                if let Some(item) = inner.next() {
                    return Some(item);
                }
                self.inner = None;
            }

            match self.outer.next() {
                Some(inner) => self.inner = Some(inner.into_iter()),
                None => return None,
            }
        }
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

    #[test]
    fn test_zip_iterator() {
        let a = Range::new(0i64, 5i64);
        let b = Range::new(10i64, 15i64);
        let mut zipped = Zip::new(a, b);

        assert_eq!(zipped.next(), Some((0, 10)));
        assert_eq!(zipped.next(), Some((1, 11)));
        assert_eq!(zipped.next(), Some((2, 12)));
        assert_eq!(zipped.next(), Some((3, 13)));
        assert_eq!(zipped.next(), Some((4, 14)));
        assert_eq!(zipped.next(), None);
    }

    #[test]
    fn test_chain_iterator() {
        let a = Range::new(0i64, 3i64);
        let b = Range::new(10i64, 13i64);
        let mut chained = Chain::new(a, b);

        assert_eq!(chained.next(), Some(0));
        assert_eq!(chained.next(), Some(1));
        assert_eq!(chained.next(), Some(2));
        assert_eq!(chained.next(), Some(10));
        assert_eq!(chained.next(), Some(11));
        assert_eq!(chained.next(), Some(12));
        assert_eq!(chained.next(), None);
    }

    #[test]
    fn test_take_iterator() {
        let iter = Range::new(0i64, 10i64);
        let mut taken = Take::new(iter, 3);

        assert_eq!(taken.next(), Some(0));
        assert_eq!(taken.next(), Some(1));
        assert_eq!(taken.next(), Some(2));
        assert_eq!(taken.next(), None);
    }

    #[test]
    fn test_skip_iterator() {
        let iter = Range::new(0i64, 10i64);
        let mut skipped = Skip::new(iter, 5);

        assert_eq!(skipped.next(), Some(5));
        assert_eq!(skipped.next(), Some(6));
        assert_eq!(skipped.next(), Some(7));
    }

    #[test]
    fn test_enumerate_iterator() {
        let iter = Range::new(10i64, 13i64);
        let mut enumerated = Enumerate::new(iter);

        assert_eq!(enumerated.next(), Some((0, 10)));
        assert_eq!(enumerated.next(), Some((1, 11)));
        assert_eq!(enumerated.next(), Some((2, 12)));
        assert_eq!(enumerated.next(), None);
    }

    #[test]
    fn test_flatten_iterator() {
        let data = vec![vec![1, 2], vec![3, 4, 5], vec![6]];
        let mut flattened = Flatten::new(data.into_iter());

        assert_eq!(flattened.next(), Some(1));
        assert_eq!(flattened.next(), Some(2));
        assert_eq!(flattened.next(), Some(3));
        assert_eq!(flattened.next(), Some(4));
        assert_eq!(flattened.next(), Some(5));
        assert_eq!(flattened.next(), Some(6));
        assert_eq!(flattened.next(), None);
    }

    #[test]
    fn test_zip_unequal_lengths() {
        let a = Range::new(0i64, 3i64);
        let b = Range::new(10i64, 15i64);
        let zipped: Vec<_> = Zip::new(a, b).collect();

        assert_eq!(zipped.len(), 3);
    }

    #[test]
    fn test_chain_size_hint() {
        let a = Range::new(0i64, 3i64);
        let b = Range::new(10i64, 15i64);
        let chained = Chain::new(a, b);

        assert_eq!(chained.size_hint(), (8, Some(8)));
    }
}

// Note: FromIterator and IntoIterator for Vec are already implemented in std
