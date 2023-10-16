use std::{
    cell::RefCell,
    collections::VecDeque,
    rc::{Rc, Weak},
};

use crate::{rvals::SteelVector, values::lists::List};
use num::BigInt;

use crate::{
    gc::{unsafe_erased_pointers::OpaqueReference, Gc},
    rvals::{
        cycles::BreadthFirstSearchSteelValVisitor, BoxedAsyncFunctionSignature,
        BuiltInDataStructureIterator, CustomType, FunctionSignature, FutureResult,
        MutFunctionSignature, SteelHashMap, SteelHashSet, SteelString, Syntax,
    },
    steel_vm::vm::{BuiltInSignature, Continuation},
    values::{
        contracts::{ContractType, FunctionKind},
        functions::ByteCodeLambda,
    },
    SteelVal,
};

use crate::rvals::SteelVal::*;

use super::{
    functions::BoxedDynFunction,
    lazy_stream::LazyStream,
    port::SteelPort,
    structs::UserDefinedStruct,
    transducers::{Reducer, Transducer},
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
    mark_and_sweep_queue: VecDeque<SteelVal>,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            memory: Vec::with_capacity(256),
            count: 0,
            threshold: GC_THRESHOLD,
            mark_and_sweep_queue: VecDeque::with_capacity(256),
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

        let mut context = MarkAndSweepContext {
            queue: &mut self.mark_and_sweep_queue,
        };

        for root in roots {
            context.push_back(root.clone());
        }

        context.visit();

        for root in globals {
            context.push_back(root.clone());
        }

        context.visit();

        for function in function_stack {
            for heap_ref in function.heap_allocated.borrow().iter() {
                context.mark_heap_reference(&heap_ref.strong_ptr())
            }
        }

        context.visit();

        ROOTS.with(|x| {
            x.borrow()
                .roots
                .values()
                .for_each(|value| context.push_back(value.clone()))
        });

        context.visit();

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
            for value in v.iter() {
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

struct MarkAndSweepContext<'a> {
    queue: &'a mut VecDeque<SteelVal>,
}

impl<'a> MarkAndSweepContext<'a> {
    fn mark_heap_reference(&mut self, heap_ref: &Rc<RefCell<HeapAllocated>>) {
        if heap_ref.borrow().is_reachable() {
            return;
        }

        {
            heap_ref.borrow_mut().mark_reachable();
        }

        self.push_back(heap_ref.borrow().value.clone());
    }
}

impl<'a> BreadthFirstSearchSteelValVisitor for MarkAndSweepContext<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    fn pop_front(&mut self) -> Option<SteelVal> {
        self.queue.pop_front()
    }

    fn push_back(&mut self, value: SteelVal) {
        self.queue.push_back(value);
    }

    fn visit_closure(&mut self, closure: Gc<ByteCodeLambda>) -> Self::Output {
        for heap_ref in closure.heap_allocated.borrow().iter() {
            self.mark_heap_reference(&heap_ref.strong_ptr())
        }

        for capture in closure.captures() {
            self.push_back(capture.clone());
        }

        if let Some(contract) = closure.get_contract_information().as_ref() {
            self.push_back(contract.clone());
        }
    }

    fn visit_bool(&mut self, _boolean: bool) -> Self::Output {}
    fn visit_float(&mut self, _float: f64) -> Self::Output {}
    fn visit_int(&mut self, _int: isize) -> Self::Output {}
    fn visit_char(&mut self, _c: char) -> Self::Output {}

    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        for value in vector.iter() {
            self.push_back(value.clone());
        }
    }

    fn visit_void(&mut self) -> Self::Output {}
    fn visit_string(&mut self, _string: SteelString) -> Self::Output {}
    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) -> Self::Output {}
    fn visit_symbol(&mut self, _symbol: SteelString) -> Self::Output {}

    fn visit_custom_type(&mut self, custom_type: Gc<RefCell<Box<dyn CustomType>>>) -> Self::Output {
        todo!()
    }

    fn visit_hash_map(&mut self, hashmap: SteelHashMap) -> Self::Output {
        for (key, value) in hashmap.iter() {
            self.push_back(key.clone());
            self.push_back(value.clone());
        }
    }

    fn visit_hash_set(&mut self, hashset: SteelHashSet) -> Self::Output {
        for value in hashset.iter() {
            self.push_back(value.clone());
        }
    }

    fn visit_steel_struct(&mut self, steel_struct: Gc<RefCell<UserDefinedStruct>>) -> Self::Output {
        for field in &steel_struct.borrow().fields {
            self.push_back(field.clone());
        }
    }

    fn visit_port(&mut self, _port: Gc<SteelPort>) -> Self::Output {}

    fn visit_transducer(&mut self, transducer: Gc<Transducer>) -> Self::Output {
        for transducer in transducer.ops.iter() {
            match transducer.clone() {
                crate::values::transducers::Transducers::Map(m) => self.push_back(m),
                crate::values::transducers::Transducers::Filter(v) => self.push_back(v),
                crate::values::transducers::Transducers::Take(t) => self.push_back(t),
                crate::values::transducers::Transducers::Drop(d) => self.push_back(d),
                crate::values::transducers::Transducers::FlatMap(fm) => self.push_back(fm),
                crate::values::transducers::Transducers::Flatten => {}
                crate::values::transducers::Transducers::Window(w) => self.push_back(w),
                crate::values::transducers::Transducers::TakeWhile(tw) => self.push_back(tw),
                crate::values::transducers::Transducers::DropWhile(dw) => self.push_back(dw),
                crate::values::transducers::Transducers::Extend(e) => self.push_back(e),
                crate::values::transducers::Transducers::Cycle => {}
                crate::values::transducers::Transducers::Enumerating => {}
                crate::values::transducers::Transducers::Zipping(z) => self.push_back(z),
                crate::values::transducers::Transducers::Interleaving(i) => self.push_back(i),
            }
        }
    }

    fn visit_reducer(&mut self, reducer: Gc<Reducer>) -> Self::Output {
        match reducer.as_ref().clone() {
            Reducer::ForEach(f) => self.push_back(f),
            Reducer::Generic(rf) => {
                self.push_back(rf.initial_value);
                self.push_back(rf.function);
            }
            _ => {}
        }
    }

    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) -> Self::Output {}
    fn visit_future(&mut self, _future: Gc<FutureResult>) -> Self::Output {}

    fn visit_stream(&mut self, stream: Gc<LazyStream>) -> Self::Output {
        self.push_back(stream.initial_value.clone());
        self.push_back(stream.stream_thunk.clone());
    }

    fn visit_contract(&mut self, contract: Gc<ContractType>) -> Self::Output {
        todo!()
    }

    fn visit_contracted_function(
        &mut self,
        function: Gc<super::contracts::ContractedFunction>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_boxed_function(&mut self, _function: Rc<BoxedDynFunction>) -> Self::Output {}

    fn visit_continuation(&mut self, continuation: Gc<Continuation>) -> Self::Output {
        for value in &continuation.stack {
            self.push_back(value.clone());
        }

        for value in &continuation.current_frame.function.captures {
            self.push_back(value.clone());
        }

        for frame in &continuation.stack_frames {
            for value in &frame.function.captures {
                self.push_back(value.clone());
            }
        }
    }

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        for value in list {
            self.push_back(value);
        }
    }

    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) -> Self::Output {}

    fn visit_mutable_vector(&mut self, vector: Gc<RefCell<Vec<SteelVal>>>) -> Self::Output {
        for value in vector.borrow_mut().iter() {
            self.push_back(value.clone());
        }
    }

    fn visit_builtin_function(&mut self, _function: BuiltInSignature) -> Self::Output {}

    // TODO: Revisit this when the boxed iterator is cleaned up
    fn visit_boxed_iterator(
        &mut self,
        iterator: Gc<RefCell<BuiltInDataStructureIterator>>,
    ) -> Self::Output {
    }

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output {
        if let Some(raw) = syntax_object.raw.clone() {
            self.push_back(raw);
        }

        self.push_back(syntax_object.syntax.clone());
    }

    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) -> Self::Output {
        self.push_back(boxed_value.borrow().clone());
    }

    // TODO: Revisit this
    fn visit_reference_value(&mut self, reference: Rc<OpaqueReference<'static>>) -> Self::Output {}

    fn visit_bignum(&mut self, _bignum: Gc<BigInt>) -> Self::Output {}
}
