use crate::rvals::UpValue;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub struct UpValueHeap {
    memory: Vec<Rc<RefCell<UpValue>>>,
}

impl UpValueHeap {
    pub fn new() -> Self {
        UpValueHeap { memory: Vec::new() }
    }

    pub(crate) fn new_upvalue(
        &mut self,
        index: usize,
        next: Option<Weak<RefCell<UpValue>>>,
    ) -> Weak<RefCell<UpValue>> {
        println!("**** Registering upvalue on the heap ****");

        let upvalue = Rc::new(RefCell::new(UpValue::new(index, next)));
        let weak_ptr = Rc::downgrade(&upvalue);
        self.memory.push(upvalue);
        weak_ptr
    }
}
