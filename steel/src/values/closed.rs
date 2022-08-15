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
    pub fn new() -> Self {
        Heap {
            memory: Vec::with_capacity(256),
            count: 0,
            threshold: 256,
        }
    }

    // Allocate this variable on the heap
    // It explicitly should no longer be on the stack, and variables that
    // reference it should be pointing here now
    pub fn allocate<'a>(
        &mut self,
        value: SteelVal,
        roots: impl Iterator<Item = &'a SteelVal>,
        live_functions: impl Iterator<Item = &'a Gc<ByteCodeLambda>>,
    ) -> HeapRef {
        let pointer = Rc::new(RefCell::new(HeapAllocated::new(value)));
        let weak_ptr = Rc::downgrade(&pointer);

        self.memory.push(pointer);

        self.collect(roots, live_functions);

        HeapRef { inner: weak_ptr }
    }

    pub fn collect<'a>(
        &mut self,
        roots: impl Iterator<Item = &'a SteelVal>,
        live_functions: impl Iterator<Item = &'a Gc<ByteCodeLambda>>,
    ) {
        println!("Calling collect - no op");
    }
}

#[derive(Clone, Debug)]
pub struct HeapRef {
    inner: Weak<RefCell<HeapAllocated>>,
}

impl HeapRef {
    pub fn get(&self) -> SteelVal {
        self.inner.upgrade().unwrap().borrow().value.clone()
    }

    pub fn set(&mut self, value: SteelVal) -> SteelVal {
        let inner = self.inner.upgrade().unwrap();

        let ret = { inner.borrow().value.clone() };

        inner.borrow_mut().value = value;
        ret
    }
}

#[derive(Clone, Debug)]
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
