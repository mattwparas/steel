use crate::{rvals::UpValue, SteelVal};
use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub struct UpValueHeap {
    memory: Vec<Rc<RefCell<UpValue>>>,
}

impl UpValueHeap {
    pub fn new() -> Self {
        UpValueHeap { memory: Vec::new() }
    }

    fn profile_heap(&self) {
        println!(
            "{:?}",
            self.memory
                .iter()
                .map(|x| Rc::weak_count(x))
                .collect::<Vec<_>>()
        );
    }

    // This does not handle cycles, perhaps add an explicit cycle detection via traversal
    // to mark things reachable?
    fn drop_weak_refs(&mut self) {
        self.memory.retain(|x| Rc::weak_count(x) > 0);
    }

    pub(crate) fn new_upvalue(
        &mut self,
        index: usize,
        next: Option<Weak<RefCell<UpValue>>>,
    ) -> Weak<RefCell<UpValue>> {
        let upvalue = Rc::new(RefCell::new(UpValue::new(index, next)));
        let weak_ptr = Rc::downgrade(&upvalue);
        self.memory.push(upvalue);

        // self.profile_heap();
        self.drop_weak_refs();

        weak_ptr
    }
}
