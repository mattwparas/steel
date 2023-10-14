use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::{
    gc::Gc,
    values::{
        contracts::{ContractType, FunctionKind},
        functions::ByteCodeLambda,
    },
    SteelVal,
};

const GC_THRESHOLD: usize = 256;
const GC_GROW_FACTOR: usize = 2;
const _RESET_LIMIT: usize = 5;

thread_local! {
    static ROOTS: RefCell<Roots> = RefCell::new(Roots::default());
}

#[derive(Default)]
pub struct Roots {
    generation: usize,
    offset: usize,
    roots: fxhash::FxHashMap<(usize, usize), SteelVal>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct RootToken {
    generation: usize,
    offset: usize,
}

impl Drop for RootToken {
    fn drop(&mut self) {
        ROOTS.with(|x| x.borrow_mut().free(self))
    }
}

#[derive(Debug)]
pub struct RootedSteelVal {
    value: SteelVal,
    token: RootToken,
}

impl RootedSteelVal {
    pub fn value(&self) -> &SteelVal {
        &self.value
    }
}

impl Roots {
    fn root(&mut self, value: SteelVal) -> RootToken {
        let generation = self.generation;
        let offset = self.offset;

        self.offset += 1;

        self.roots.insert((generation, offset), value);

        RootToken { generation, offset }
    }

    fn free(&mut self, token: &RootToken) {
        self.roots.remove(&(token.generation, token.offset));
    }

    fn increment_generation(&mut self) {
        self.generation += 1;
    }
}

impl SteelVal {
    pub fn mark_rooted(&self) -> RootToken {
        ROOTS.with(|x| x.borrow_mut().root(self.clone()))
    }

    // If we're storing in an external struct that could escape
    // the runtime, we probably want to be marked as rooted
    pub fn as_rooted(&self) -> RootedSteelVal {
        let token = self.mark_rooted();

        RootedSteelVal {
            token,
            value: self.clone(),
        }
    }
}

#[derive(Clone)]
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
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: impl Iterator<Item = &'a SteelVal>,
    ) -> HeapRef {
        self.collect(roots, live_functions, globals);

        let pointer = Rc::new(RefCell::new(HeapAllocated::new(value)));
        let weak_ptr = Rc::downgrade(&pointer);

        self.memory.push(pointer);

        HeapRef { inner: weak_ptr }
    }

    pub fn collect<'a>(
        &mut self,
        roots: impl Iterator<Item = &'a SteelVal>,
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: impl Iterator<Item = &'a SteelVal>,
    ) {
        if self.memory.len() > self.threshold {
            log::info!(target: "gc", "Freeing memory");

            let mut changed = true;
            while changed {
                log::info!(target: "gc", "Small collection");
                let prior_len = self.memory.len();
                log::info!(target: "gc", "Previous length: {:?}", prior_len);
                self.memory.retain(|x| Rc::weak_count(x) > 0);
                let after = self.memory.len();
                log::info!(target: "gc", "Objects freed: {:?}", prior_len - after);
                changed = prior_len != after;
            }

            // TODO fix the garbage collector
            self.mark_and_sweep(roots, live_functions, globals);

            self.threshold = (self.threshold + self.memory.len()) * GC_GROW_FACTOR;

            self.count += 1;
        }
    }

    fn mark_and_sweep<'a>(
        &mut self,
        roots: impl Iterator<Item = &'a SteelVal>,
        function_stack: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: impl Iterator<Item = &'a SteelVal>,
    ) {
        log::info!(target: "gc", "Marking the heap");

        // mark
        for root in roots {
            traverse(root);
        }

        for root in globals {
            traverse(root);
        }

        for function in function_stack {
            for heap_ref in function.heap_allocated.borrow().iter() {
                mark_heap_ref(&heap_ref.strong_ptr())
            }
        }

        ROOTS.with(|x| x.borrow().roots.values().for_each(traverse));

        // println!("Freeing heap");

        // TODO -> move destructors to another thread?
        // That way the main thread is not blocked by the dropping of unreachable objects

        // println!(
        //     "Dropping memory: {:?}",
        //     self.memory
        //         .iter()
        //         .filter(|x| !x.borrow().is_reachable())
        //         .map(|x| (Rc::weak_count(&x), x))
        //         .collect::<Vec<_>>()
        // );

        log::info!(target: "gc", "Sweeping");
        let prior_len = self.memory.len();

        // sweep
        self.memory.retain(|x| x.borrow().is_reachable());

        let after_len = self.memory.len();

        let amount_freed = prior_len - after_len;

        log::info!(target: "gc", "Freed objects: {:?}", amount_freed);

        // put them back as unreachable
        self.memory.iter().for_each(|x| x.borrow_mut().reset());

        ROOTS.with(|x| x.borrow_mut().increment_generation());
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

    pub(crate) fn set_interior_mut(&self, value: SteelVal) -> SteelVal {
        let inner = self.inner.upgrade().unwrap();

        let ret = { inner.borrow().value.clone() };

        inner.borrow_mut().value = value;
        ret
    }

    fn strong_ptr(&self) -> Rc<RefCell<HeapAllocated>> {
        self.inner.upgrade().unwrap()
    }
}

// impl AsRefSteelVal for HeapRef {
//     type Nursery = ();

//     fn as_ref<'b, 'a: 'b>(
//         val: &'a SteelVal,
//         _nursery: &mut Self::Nursery,
//     ) -> crate::rvals::Result<SRef<'b, Self>> {
//         if let SteelVal::Boxed(s) = val {
//             Ok(SRef::Temporary(s))
//         } else {
//             stop!(TypeMismatch => "Value cannot be referenced as a syntax object")
//         }
//     }
// }

#[derive(Clone, Debug, PartialEq, Eq)]
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

pub struct HeapContext;

impl HeapContext {
    pub fn visit(&mut self, val: &SteelVal) {
        traverse(val)
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
        SteelVal::MutableVector(v) => {
            for value in v.borrow().iter() {
                traverse(value)
            }
        }
        SteelVal::HashMapV(h) => {
            for (key, value) in h.iter() {
                traverse(key);
                traverse(value);
            }
        }
        SteelVal::HashSetV(s) => {
            for key in s.iter() {
                traverse(key);
            }
        }
        // SteelVal::StructV(_) => {}
        // SteelVal::PortV(_) => {}
        SteelVal::Closure(c) => {
            for heap_ref in c.heap_allocated.borrow().iter() {
                mark_heap_ref(&heap_ref.strong_ptr())
            }

            for capture in c.captures() {
                traverse(capture);
            }

            if let Some(contract) = c.get_contract_information().as_ref() {
                traverse(contract);
            }
        }
        // SteelVal::IterV(_) => {}
        // SteelVal::FutureV(_) => {}
        SteelVal::StreamV(s) => {
            traverse(&s.initial_value);
            traverse(&s.stream_thunk);
        }
        // SteelVal::BoxV(_) => {}
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

            for function in c.stack_frames.iter().map(|x| &x.function) {
                for heap_ref in function.heap_allocated.borrow().iter() {
                    mark_heap_ref(&heap_ref.strong_ptr())
                }

                for capture in function.captures() {
                    traverse(capture);
                }
            }
        }
        SteelVal::BoolV(_) => {}
        SteelVal::NumV(_) => {}
        SteelVal::IntV(_) => {}
        SteelVal::CharV(_) => {}
        SteelVal::Void => {}
        SteelVal::StringV(_) => {}
        SteelVal::FuncV(_) => {}
        SteelVal::SymbolV(_) => {}
        SteelVal::Custom(c) => c.borrow().visit(&mut HeapContext),
        SteelVal::CustomStruct(_) => todo!(),
        SteelVal::PortV(_) => todo!(),
        SteelVal::IterV(_) => todo!(),
        SteelVal::ReducerV(_) => todo!(),
        SteelVal::FutureFunc(_) => todo!(),
        SteelVal::FutureV(_) => todo!(),
        SteelVal::BoxedFunction(_) => todo!(),
        SteelVal::MutFunc(_) => todo!(),
        SteelVal::BuiltIn(_) => todo!(),
        SteelVal::BoxedIterator(_) => todo!(),
        SteelVal::SyntaxObject(_) => todo!(),
        SteelVal::Boxed(_) => todo!(),
        SteelVal::Reference(_) => todo!(),
        SteelVal::BigNum(_) => todo!(),
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

    for capture in c.captures() {
        traverse(capture);
    }
}

fn mark_heap_ref(heap_ref: &Rc<RefCell<HeapAllocated>>) {
    if heap_ref.borrow().is_reachable() {
        return;
    }

    {
        heap_ref.borrow_mut().mark_reachable();
    }

    traverse(&heap_ref.borrow().value);
}
