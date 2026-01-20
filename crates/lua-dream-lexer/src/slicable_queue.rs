use std::{
    mem::MaybeUninit,
    ops::{Index, Range, RangeFrom, RangeTo},
};

/// Queue that only resets the vector, if totally drained out
#[derive(Debug)]
pub struct SlicableQueue<T> {
    content: Vec<MaybeUninit<T>>,
    start_index: usize,
}

impl<T> Drop for SlicableQueue<T> {
    fn drop(&mut self) {
        for elem in &mut self.content[self.start_index..] {
            // SAFETY: Only elems before start_index are uninit
            unsafe {
                elem.assume_init_drop();
            }
        }
    }
}

impl<T: Clone> Clone for SlicableQueue<T> {
    fn clone(&self) -> Self {
        Self {
            content: self.content[self.start_index..]
                .iter()
                .map(|c| {
                    // SAFETY: we only clone the part that has not been popped yet
                    let c = unsafe { c.assume_init_ref() };
                    MaybeUninit::new(c.clone())
                })
                .collect(),
            start_index: 0,
        }
    }
}

impl<T> From<Vec<T>> for SlicableQueue<T> {
    fn from(value: Vec<T>) -> Self {
        Self {
            content: value.into_iter().map(|v| MaybeUninit::new(v)).collect(),
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

impl<T> SlicableQueue<T> {
    pub fn pop_front(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }

        let mut elem = MaybeUninit::uninit();

        std::mem::swap(&mut elem, &mut self.content[self.start_index]);

        // SAFETY: element has not been popped yet, as that is only the case for elements before start_index
        let res = unsafe { elem.assume_init() };
        self.start_index += 1;
        if self.start_index >= self.content.len() {
            // Hold up invariant: if queue is empty,
            // then content is also empty and the other way round
            self.clear();
        }
        Some(res)
    }

    pub fn push_back(&mut self, elem: T) {
        self.content.push(MaybeUninit::new(elem))
    }

    pub fn as_slice(&self) -> &[T] {
        let slice = &self.content[self.start_index..];
        // SAFETY: All elements after start_index including the element at start_index are still initialized
        unsafe { std::slice::from_raw_parts(slice.as_ptr() as *const T, slice.len()) }
    }

    pub fn len(&self) -> usize {
        self.content.len() - self.start_index
    }

    pub fn is_empty(&self) -> bool {
        // Using invariant, that if content is empty, then the queue is empty
        self.content.is_empty()
    }

    pub fn clear(&mut self) {
        self.content.clear();
        self.start_index = 0;
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
