use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::{
    gc::Gc,
    values::{
        contracts::{ContractType, FunctionKind},
        functions::ByteCodeLambda,
        upvalue::UpValue,
    },
    SteelVal,
};

const GC_THRESHOLD: usize = 256;
const GC_GROW_FACTOR: usize = 2;
const _RESET_LIMIT: usize = 5;

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
            threshold: GC_THRESHOLD,
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
        if self.memory.len() > self.threshold {
            // println!("Freeing memory");

            let mut changed = true;
            while changed {
                let prior_len = self.memory.len();
                self.memory.retain(|x| Rc::weak_count(x) > 0);
                let after = self.memory.len();
                changed = prior_len != after;
            }

            // TODO fix the garbage collector
            self.mark_and_sweep(roots, live_functions);

            self.threshold = (self.threshold + self.memory.len()) * GC_GROW_FACTOR;

            self.count += 1;
        }
    }

    fn mark_and_sweep<'a>(
        &mut self,
        roots: impl Iterator<Item = &'a SteelVal>,
        function_stack: impl Iterator<Item = &'a Gc<ByteCodeLambda>>,
    ) {
        // mark
        for root in roots {
            traverse(root);
        }

        for function in function_stack {
            for heap_ref in function.heap_allocated.borrow().iter() {
                mark_heap_ref(&heap_ref.strong_ptr())
            }
        }

        // println!("Freeing heap");

        // TODO -> move destructors to another thread?
        // That way the main thread is not blocked by the dropping of unreachable objects

        // sweep
        self.memory.retain(|x| x.borrow().is_reachable());

        // put them back as unreachable
        self.memory.iter().for_each(|x| x.borrow_mut().reset());
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

    fn strong_ptr(&self) -> Rc<RefCell<HeapAllocated>> {
        self.inner.upgrade().unwrap()
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

    pub fn is_reachable(&self) -> bool {
        self.reachable
    }

    pub(crate) fn mark_reachable(&mut self) {
        self.reachable = true;
    }

    pub(crate) fn reset(&mut self) {
        self.reachable = false;
    }
}

// Use this function to traverse and find all reachable things
// 'reachable' should be values living in the heap, stack, and in the
fn traverse(val: &SteelVal) {
    match val {
        // SteelVal::Pair(_) => {}
        SteelVal::VectorV(v) => {
            for value in v.iter() {
                traverse(value)
            }
        }
        SteelVal::ListV(v) => {
            for value in v {
                traverse(value)
            }
        }
        SteelVal::HashMapV(_) => {}
        SteelVal::HashSetV(_) => {}
        SteelVal::StructV(_) => {}
        SteelVal::PortV(_) => {}
        SteelVal::Closure(c) => {
            for heap_ref in c.heap_allocated.borrow().iter() {
                mark_heap_ref(&heap_ref.strong_ptr())
            }
        }
        SteelVal::IterV(_) => {}
        SteelVal::FutureV(_) => {}
        SteelVal::StreamV(s) => {
            traverse(&s.initial_value);
            traverse(&s.stream_thunk);
        }
        SteelVal::BoxV(_) => {}
        SteelVal::Contract(c) => visit_contract_type(c),
        SteelVal::ContractedFunction(c) => {
            visit_function_contract(&c.contract);
            if let SteelVal::Closure(func) = &c.function {
                visit_closure(func);
            }
            // visit_closure(&c.function);
        }
        SteelVal::ContinuationFunction(c) => {
            for root in c.stack.iter() {
                traverse(root);
            }

            for function in c.function_stack.iter() {
                for heap_ref in function.heap_allocated.borrow().iter() {
                    mark_heap_ref(&heap_ref.strong_ptr())
                }
            }
        }
        _ => {}
    }
}

fn visit_function_contract(f: &FunctionKind) {
    match f {
        FunctionKind::Basic(f) => {
            for pre_condition in f.pre_conditions() {
                visit_contract_type(pre_condition)
            }
            visit_contract_type(f.post_condition());
        }
        FunctionKind::Dependent(_dc) => {
            unimplemented!()
        }
    }
}

fn visit_contract_type(contract: &ContractType) {
    match contract {
        ContractType::Flat(f) => {
            traverse(f.predicate());
        }
        ContractType::Function(f) => {
            visit_function_contract(f);
        }
    }
}

fn visit_closure(c: &Gc<ByteCodeLambda>) {
    for heap_ref in c.heap_allocated.borrow().iter() {
        mark_heap_ref(&heap_ref.strong_ptr());
    }
}

#[inline(always)]
fn mark_heap_ref(heap_ref: &Rc<RefCell<HeapAllocated>>) {
    if heap_ref.borrow().is_reachable() {
        return;
    }

    {
        heap_ref.borrow_mut().mark_reachable();
    }

    traverse(&heap_ref.borrow().value);
}
