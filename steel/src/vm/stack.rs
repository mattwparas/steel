use crate::env::Env;
use crate::rvals::SteelVal;
use std::cell::RefCell;
use std::ops::RangeFrom;
use std::rc::Rc;

use crate::gc::Gc;

pub type CallStack = Stack<Stack<Gc<SteelVal>>>;
pub type StackFrame = Stack<Gc<SteelVal>>;
pub type EnvStack = Stack<Rc<RefCell<Env>>>;

#[derive(Debug)]
pub struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack(Vec::new())
    }

    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub fn try_pop(&mut self) -> Option<T> {
        self.0.pop()
    }

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

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn as_slice(&self) -> &[T] {
        self.0.as_slice()
    }
}

impl<T> From<Vec<T>> for Stack<T> {
    fn from(val: Vec<T>) -> Stack<T> {
        Stack(val)
    }
}
