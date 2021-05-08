use crate::{rvals::UpValue, SteelVal};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

const GC_THRESHOLD: usize = 100;
const GC_GROW_FACTOR: usize = 2;
const RESET_LIMIT: usize = 5;

pub struct UpValueHeap {
    memory: Vec<Rc<RefCell<UpValue>>>,
    count: usize,
    threshold: usize,
}

impl UpValueHeap {
    pub fn new() -> Self {
        UpValueHeap {
            memory: Vec::new(),
            count: 1,
            threshold: GC_THRESHOLD,
        }
    }

    fn profile_heap(&self) {
        let mapped = self
            .memory
            .iter()
            .map(|x| Rc::weak_count(x))
            .collect::<Vec<_>>();
        let mut hm: HashMap<usize, usize> = HashMap::new();
        for value in mapped {
            // hm.insert(k, v)
            let count = hm.entry(value).or_insert(0);
            *count += 1;
        }

        println!("{:#?}", hm);
    }

    // This does not handle cycles, perhaps add an explicit cycle detection via traversal
    // to mark things reachable?
    fn collect(&mut self) {
        if self.memory.len() > self.threshold {
            let prior = self.memory.len();

            let mut changed = true;
            while changed {
                let prior_len = self.memory.len();
                self.memory.retain(|x| Rc::weak_count(x) > 0);
                let after = self.memory.len();
                changed = prior_len != after;
            }
            // self.memory.retain(|x| Rc::weak_count(x) > 0);

            // let after = self.memory.len();
            self.threshold = self.memory.len() * GC_GROW_FACTOR;

            // self.profile_heap();
            // println!(
            //     "Freed: {}, New heap length: {}",
            //     prior - after,
            //     self.memory.len()
            // );
            self.count += 1;
        }
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
        self.collect();
        // self.profile_heap();

        weak_ptr
    }
}
