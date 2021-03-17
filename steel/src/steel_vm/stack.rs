use crate::env::Env;
use crate::rvals::SteelVal;
use std::ops::RangeFrom;
use std::rc::Rc;
use std::{cell::RefCell, ops::Deref};

pub type CallStack = Stack<Stack<SteelVal>>;
pub type StackFrame = Stack<SteelVal>;
pub type EnvStack = Stack<Rc<RefCell<Env>>>;

#[derive(Debug, Clone)]
pub struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    // #[inline(always)]
    pub fn new() -> Stack<T> {
        Stack(Vec::new())
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

    pub fn peek_range(&self, range: RangeFrom<usize>) -> &[T] {
        &self.0[range]
    }

    pub fn truncate(&mut self, idx: usize) {
        self.0.truncate(idx)
    }

    pub fn clear(&mut self) {
        self.0.clear();
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
