use crate::rvals::SteelVal;
use std::ops::Deref;
use std::ops::RangeFrom;

pub type StackFrame = Stack<SteelVal>;

#[derive(Debug, Clone)]
pub struct Stack<T>(pub(crate) Vec<T>);

impl<T> Stack<T> {
    // #[inline(always)]
    pub fn new() -> Stack<T> {
        Stack(Vec::new())
    }

    pub fn with_capacity(capacity: usize) -> Stack<T> {
        Stack(Vec::with_capacity(capacity))
    }

    // #[inline(always)]
    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    // #[inline(always)]
    pub fn try_pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    // #[inline(always)]
    pub fn push(&mut self, value: T) {
        self.0.push(value);
    }

    pub fn split_off(&mut self, idx: usize) -> Vec<T> {
        self.0.split_off(idx)
    }

    pub fn peek_range_mut(&mut self, range: RangeFrom<usize>) -> &mut [T] {
        &mut self.0[range]
    }

    pub fn peek_range(&self, range: RangeFrom<usize>) -> &[T] {
        &self.0[range]
    }

    pub fn peek_range_double(&self, range: std::ops::Range<usize>) -> &[T] {
        &self.0[range]
    }

    pub fn truncate(&mut self, idx: usize) {
        self.0.truncate(idx)
    }

    pub fn drain_range(&mut self, range: std::ops::Range<usize>) {
        self.0.drain(range);
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn append_vec(&mut self, other: &mut Vec<T>) {
        self.0.append(other)
    }

    #[inline(always)]
    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.0.last_mut()
    }

    #[inline(always)]
    pub fn set_idx(&mut self, idx: usize, value: T) {
        self.0[idx] = value;
    }
}

impl<T> From<Vec<T>> for Stack<T> {
    fn from(val: Vec<T>) -> Stack<T> {
        Stack(val)
    }
}

impl<T> Deref for Stack<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.0
    }
}
