use std::ops::{Index, Range, RangeFrom, RangeTo};

/// Queue that only resets the vector, if totally drained out
#[derive(Debug, Clone)]
pub struct SlicableQueue<T> {
    // We could use MaybeUninit in the future
    content: Vec<T>,
    start_index: usize,
}

impl<T> From<Vec<T>> for SlicableQueue<T> {
    fn from(value: Vec<T>) -> Self {
        Self {
            content: value,
            ..Default::default()
        }
    }
}

impl<T> Default for SlicableQueue<T> {
    fn default() -> Self {
        Self {
            content: Default::default(),
            start_index: Default::default(),
        }
    }
}

impl<T: Copy> SlicableQueue<T> {
    pub fn pop_front(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }

        let res = self.content[self.start_index];
        self.start_index += 1;
        if self.start_index >= self.content.len() {
            // Keep up invariant: if queue is empty,
            // then content is also empty and the other way round
            self.content.clear();
            self.start_index = 0;
        }
        Some(res)
    }
}

impl<T> SlicableQueue<T> {
    pub fn push_back(&mut self, elem: T) {
        self.content.push(elem)
    }

    pub fn as_slice(&self) -> &[T] {
        &self.content[self.start_index..]
    }

    pub fn len(&self) -> usize {
        self.content.len() - self.start_index
    }

    pub fn is_empty(&self) -> bool {
        // Using invariant, that if content is empty, then the queue is empty
        self.content.is_empty()
    }
}

impl<T> Index<Range<usize>> for SlicableQueue<T> {
    type Output = [T];
    fn index(&self, range: Range<usize>) -> &Self::Output {
        &self.as_slice()[range]
    }
}

impl<T> Index<RangeTo<usize>> for SlicableQueue<T> {
    type Output = [T];
    fn index(&self, range_to: RangeTo<usize>) -> &Self::Output {
        &self.as_slice()[range_to]
    }
}

impl<T> Index<RangeFrom<usize>> for SlicableQueue<T> {
    type Output = [T];
    fn index(&self, range_from: RangeFrom<usize>) -> &Self::Output {
        &self.as_slice()[range_from]
    }
}
