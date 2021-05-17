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
    fn collect<'a>(&mut self, roots: impl Iterator<Item = &'a SteelVal>) {
        if self.memory.len() > self.threshold {
            // let prior = self.memory.len();

            let mut changed = true;
            while changed {
                let prior_len = self.memory.len();
                self.memory.retain(|x| Rc::weak_count(x) > 0);
                let after = self.memory.len();
                changed = prior_len != after;
            }
            // self.memory.retain(|x| Rc::weak_count(x) > 0);

            self.mark_and_sweep(roots);

            // let after = self.memory.len();

            // TODO check this
            self.threshold = (self.threshold + self.memory.len()) * GC_GROW_FACTOR;

            // println!("new threshold: {}", self.threshold);

            // self.profile_heap();
            // println!(
            //     "Freed: {}, New heap length: {}",
            //     prior - after,
            //     self.memory.len()
            // );
            self.count += 1;
        }
    }

    fn mark_and_sweep<'a>(&mut self, roots: impl Iterator<Item = &'a SteelVal>) {
        // mark
        for root in roots {
            traverse(root);
        }

        // sweep
        self.memory
            .retain(|x| x.borrow().is_reachable() || !x.borrow().is_closed());

        // put them back as unreachable
        self.memory.iter().for_each(|x| x.borrow_mut().reset());
    }

    pub(crate) fn new_upvalue<'a>(
        &mut self,
        index: usize,
        next: Option<Weak<RefCell<UpValue>>>,
        roots: impl Iterator<Item = &'a SteelVal>,
    ) -> Weak<RefCell<UpValue>> {
        let upvalue = Rc::new(RefCell::new(UpValue::new(index, next)));
        let weak_ptr = Rc::downgrade(&upvalue);
        self.memory.push(upvalue);

        // self.profile_heap();
        self.collect(roots);
        // self.profile_heap();

        weak_ptr
    }
}

// Use this function to traverse and find all reachable things
// 'reachable' should be values living in the heap, stack, and in the
fn traverse(val: &SteelVal) {
    match val {
        SteelVal::Pair(_) => {}
        SteelVal::VectorV(_) => {}
        SteelVal::HashMapV(_) => {}
        SteelVal::HashSetV(_) => {}
        SteelVal::StructV(_) => {}
        SteelVal::PortV(_) => {}
        SteelVal::Closure(c) => {
            for upvalue in c.upvalues() {
                let upvalue = upvalue.upgrade().unwrap();
                mark_upvalue(&upvalue);
            }
        }
        SteelVal::IterV(_) => {}
        SteelVal::FutureV(_) => {}
        SteelVal::StreamV(_) => {}
        SteelVal::BoxV(_) => {}
        SteelVal::Contract(_) => {}
        SteelVal::ContractedFunction(_) => {}
        SteelVal::ContinuationFunction(_) => {}
        _ => {
            // println!("Doesn't need to be traversed")
        }
    }
}

#[inline(always)]
fn mark_upvalue(upvalue: &Rc<RefCell<UpValue>>) {
    // unimplemented!()
    {
        upvalue.borrow_mut().mark_reachable();
    }

    if let Some(inner) = upvalue.borrow().get_value_if_closed() {
        traverse(inner);
    }
}
