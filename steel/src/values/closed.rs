use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::{gc::Gc, SteelVal};

use super::functions::ByteCodeLambda;

pub struct Heap {
    memory: Vec<Rc<RefCell<HeapAllocated>>>,
    count: usize,
    threshold: usize,
}

impl Heap {
    // Allocate this variable on the heap
    // It explicitly should no longer be on the stack, and variables that
    // reference it should be pointing here now
    pub fn allocate<'a>(
        &mut self,
        value: SteelVal,
        roots: impl Iterator<Item = &'a SteelVal>,
        live_functions: impl Iterator<Item = &'a Gc<ByteCodeLambda>>,
    ) -> Weak<RefCell<HeapAllocated>> {
        let pointer = Rc::new(RefCell::new(HeapAllocated::new(value)));
        let weak_ptr = Rc::downgrade(&pointer);

        self.memory.push(pointer);

        self.collect(roots, live_functions);

        weak_ptr
    }

    pub fn collect<'a>(
        &mut self,
        roots: impl Iterator<Item = &'a SteelVal>,
        live_functions: impl Iterator<Item = &'a Gc<ByteCodeLambda>>,
    ) {
        todo!()
    }
}

pub struct HeapRef {
    inner: Weak<RefCell<HeapAllocated>>,
}

impl HeapRef {
    pub fn get(&self) -> SteelVal {
        self.inner.upgrade().unwrap().borrow().value.clone()
    }

    pub fn set(&mut self, value: SteelVal) {
        self.inner.upgrade().unwrap().borrow_mut().value = value
    }
}

pub struct HeapAllocated {
    pub(crate) reachable: bool,
    pub(crate) value: SteelVal,
}

impl HeapAllocated {
    pub fn new(value: SteelVal) -> Self {
        Self {
            reachable: false,
            value,
        }
    }
}
