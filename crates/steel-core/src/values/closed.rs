use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::{
    rvals::{OpaqueIterator, SteelVector},
    steel_vm::vm::{Continuation, ContinuationMark},
    values::lists::List,
};
use num::BigInt;

use crate::{
    gc::{unsafe_erased_pointers::OpaqueReference, Gc},
    rvals::{
        cycles::BreadthFirstSearchSteelValVisitor, BoxedAsyncFunctionSignature, CustomType,
        FunctionSignature, FutureResult, MutFunctionSignature, SteelHashMap, SteelHashSet,
        SteelString, Syntax,
    },
    steel_vm::vm::BuiltInSignature,
    values::functions::ByteCodeLambda,
    SteelVal,
};

use super::{
    functions::BoxedDynFunction,
    lazy_stream::LazyStream,
    port::SteelPort,
    structs::UserDefinedStruct,
    transducers::{Reducer, Transducer},
};

const GC_THRESHOLD: usize = 256 * 1000;
const GC_GROW_FACTOR: usize = 2;
const RESET_LIMIT: usize = 5;

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

type HeapValue = Rc<RefCell<HeapAllocated<SteelVal>>>;
type HeapVector = Rc<RefCell<HeapAllocated<Vec<SteelVal>>>>;

// Maybe uninitialized

struct FreeList {
    elements: Vec<Option<HeapValue>>,
    cursor: usize,
    alloc_count: usize,
}

impl FreeList {
    const EXTEND_CHUNK: usize = 128;

    fn is_heap_full(&self) -> bool {
        self.alloc_count == self.elements.len()
    }

    fn extend_heap(&mut self) {
        self.cursor = self.elements.len();

        self.elements.reserve(Self::EXTEND_CHUNK);
        self.elements
            .extend(std::iter::repeat(None).take(Self::EXTEND_CHUNK));
    }

    fn allocate(&mut self, value: SteelVal) -> HeapRef<SteelVal> {
        // Drain, moving values around...
        // is that expensive?

        let pointer = Rc::new(RefCell::new(HeapAllocated::new(value)));
        let weak_ptr = Rc::downgrade(&pointer);

        self.elements[self.cursor] = Some(pointer);
        self.alloc_count += 1;

        // Find where to assign the next slot optimistically
        let next_slot = self.elements[self.cursor..]
            .iter()
            .position(Option::is_none);

        if let Some(next_slot) = next_slot {
            self.cursor += next_slot;
        } else {
            //
            if self.is_heap_full() {
                // Extend the heap, move the cursor to the end
                self.extend_heap();
            } else {
                self.cursor = self.elements.iter().position(Option::is_none).unwrap()
            }
        }

        HeapRef { inner: weak_ptr }
    }

    fn collect_on_condition(&mut self, func: fn(&HeapValue) -> bool) -> usize {
        let mut amount_dropped = 0;

        self.elements.iter_mut().for_each(|x| {
            if x.as_ref().map(func).unwrap_or_default() {
                *x = None;
                amount_dropped += 1;
            }
        });

        self.alloc_count -= amount_dropped;

        amount_dropped
    }

    fn weak_collection(&mut self) -> usize {
        self.collect_on_condition(|inner| Rc::weak_count(inner) == 0)
    }

    fn strong_collection(&mut self) -> usize {
        self.collect_on_condition(|inner| !inner.borrow().is_reachable())
    }
}

#[derive(Clone)]
pub struct Heap {
    memory: Vec<Rc<RefCell<HeapAllocated<SteelVal>>>>,
    vectors: Vec<Rc<RefCell<HeapAllocated<Vec<SteelVal>>>>>,
    count: usize,
    threshold: usize,
    // mark_and_sweep_queue: VecDeque<SteelVal>,
    mark_and_sweep_queue: Vec<SteelVal>,
    maybe_memory_size: usize,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            memory: Vec::with_capacity(256),
            vectors: Vec::with_capacity(256),
            count: 0,
            threshold: GC_THRESHOLD,
            // mark_and_sweep_queue: VecDeque::with_capacity(256),
            mark_and_sweep_queue: Vec::with_capacity(256),
            maybe_memory_size: 0,
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
    ) -> HeapRef<SteelVal> {
        self.collect(Some(value.clone()), None, roots, live_functions, globals);

        let pointer = Rc::new(RefCell::new(HeapAllocated::new(value)));
        let weak_ptr = Rc::downgrade(&pointer);

        self.memory.push(pointer);

        HeapRef { inner: weak_ptr }
    }

    // Allocate a vector explicitly onto the heap
    pub fn allocate_vector<'a>(
        &mut self,
        values: Vec<SteelVal>,
        roots: impl Iterator<Item = &'a SteelVal>,
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: impl Iterator<Item = &'a SteelVal>,
    ) -> HeapRef<Vec<SteelVal>> {
        self.collect(None, Some(&values), roots, live_functions, globals);

        let pointer = Rc::new(RefCell::new(HeapAllocated::new(values)));
        let weak_ptr = Rc::downgrade(&pointer);

        self.vectors.push(pointer);

        HeapRef { inner: weak_ptr }
    }

    fn vector_cells_allocated(&self) -> usize {
        // self.vectors.iter().map(|x| x.borrow().value.len()).sum()
        self.vectors.len()
    }

    // TODO: Call this in more areas in the VM to attempt to free memory more carefully
    // Also - come up with generational scheme if possible
    pub fn collect<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: Option<&Vec<SteelVal>>,
        roots: impl Iterator<Item = &'a SteelVal>,
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: impl Iterator<Item = &'a SteelVal>,
    ) {
        let memory_size = self.memory.len() + self.vector_cells_allocated();

        if memory_size > self.threshold {
            log::debug!(target: "gc", "Freeing memory");

            let original_length = memory_size;

            // Do at least one small collection, where we immediately drop
            // anything that has weak counts of 0, meaning there are no alive
            // references and we can avoid doing a full collection
            //
            // In the event that the collection does not yield a substantial
            // change in the heap size, we should also enqueue a larger mark and
            // sweep collection.
            let mut changed = true;
            while changed {
                let now = std::time::Instant::now();

                log::debug!(target: "gc", "Small collection");
                let prior_len = self.memory.len() + self.vector_cells_allocated();
                log::debug!(target: "gc", "Previous length: {:?}", prior_len);
                self.memory.retain(|x| Rc::weak_count(x) > 0);
                self.vectors.retain(|x| Rc::weak_count(x) > 0);
                let after = self.memory.len() + self.vector_cells_allocated();
                log::debug!(target: "gc", "Objects freed: {:?}", prior_len - after);
                log::debug!(target: "gc", "Small collection time: {:?}", now.elapsed());

                changed = prior_len != after;
            }

            let post_small_collection_size = self.memory.len() + self.vector_cells_allocated();

            // Mark + Sweep!
            if post_small_collection_size as f64 > (0.25 * original_length as f64) {
                log::debug!(target: "gc", "---- Post small collection, running mark and sweep - heap size filled: {:?} ----", post_small_collection_size as f64 / original_length as f64);

                // TODO fix the garbage collector
                self.mark_and_sweep(root_value, root_vector, roots, live_functions, globals);
            } else {
                log::debug!(target: "gc", "---- Skipping mark and sweep - heap size filled: {:?} ----", post_small_collection_size as f64 / original_length as f64);
            }

            // self.mark_and_sweep(roots, live_functions, globals);

            self.threshold = (self.threshold + self.memory.len() + self.vector_cells_allocated())
                * GC_GROW_FACTOR;

            self.count += 1;

            // Drive it down!
            if self.count > RESET_LIMIT {
                log::debug!(target: "gc", "Shrinking the heap");

                self.threshold = GC_THRESHOLD;
                self.count = 0;

                self.memory.shrink_to(GC_THRESHOLD * GC_GROW_FACTOR);
                self.vectors.shrink_to(GC_THRESHOLD * GC_GROW_FACTOR);
            }
        }
    }

    fn mark_and_sweep<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: Option<&Vec<SteelVal>>,
        roots: impl Iterator<Item = &'a SteelVal>,
        function_stack: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: impl Iterator<Item = &'a SteelVal>,
    ) {
        log::debug!(target: "gc", "Marking the heap");

        let now = std::time::Instant::now();

        let mut context = MarkAndSweepContext {
            queue: &mut self.mark_and_sweep_queue,
        };

        if let Some(root_value) = root_value {
            context.push_back(root_value);
        }

        if let Some(root_vector) = root_vector {
            for value in root_vector {
                context.push_back(value.clone());
            }
        }

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

            for value in function.captures() {
                context.push_back(value.clone());
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

        log::debug!(target: "gc", "Mark: Time taken: {:?}", now.elapsed());

        let now = std::time::Instant::now();

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

        log::debug!(target: "gc", "--- Sweeping ---");
        let prior_len = self.memory.len() + self.vector_cells_allocated();

        // sweep
        self.memory.retain(|x| {
            // let mut guard = x.borrow_mut();
            // let is_reachable = guard.is_reachable();
            // guard.reset();
            // is_reachable

            x.borrow().is_reachable()
        });
        self.vectors.retain(|x| {
            // let mut guard = x.borrow_mut();
            // let is_reachable = guard.is_reachable();
            // guard.reset();
            // is_reachable
            x.borrow().is_reachable()
        });
        // (|x| x.borrow().is_reachable());

        let after_len = self.memory.len();

        let amount_freed = prior_len - after_len;

        log::debug!(target: "gc", "Freed objects: {:?}", amount_freed);
        log::debug!(target: "gc", "Objects alive: {:?}", after_len);

        // put them back as unreachable
        self.memory.iter().for_each(|x| x.borrow_mut().reset());
        self.vectors.iter().for_each(|x| x.borrow_mut().reset());

        ROOTS.with(|x| x.borrow_mut().increment_generation());

        log::debug!(target: "gc", "Sweep: Time taken: {:?}", now.elapsed());
    }
}

pub trait HeapAble: Clone + std::fmt::Debug + PartialEq + Eq {}
impl HeapAble for SteelVal {}
impl HeapAble for Vec<SteelVal> {}

#[derive(Clone, Debug)]
pub struct HeapRef<T: HeapAble> {
    inner: Weak<RefCell<HeapAllocated<T>>>,
}

impl<T: HeapAble> HeapRef<T> {
    pub fn get(&self) -> T {
        self.inner.upgrade().unwrap().borrow().value.clone()
    }

    pub fn as_ptr_usize(&self) -> usize {
        self.inner.as_ptr() as usize
    }

    pub fn set(&mut self, value: T) -> T {
        let inner = self.inner.upgrade().unwrap();

        let ret = { inner.borrow().value.clone() };

        inner.borrow_mut().value = value;
        ret
    }

    pub fn set_and_return(&self, value: T) -> T {
        let inner = self.inner.upgrade().unwrap();

        let mut guard = inner.borrow_mut();
        std::mem::replace(&mut guard.value, value)
    }

    pub(crate) fn set_interior_mut(&self, value: T) -> T {
        let inner = self.inner.upgrade().unwrap();

        let ret = { inner.borrow().value.clone() };

        inner.borrow_mut().value = value;
        ret
    }

    pub(crate) fn strong_ptr(&self) -> Rc<RefCell<HeapAllocated<T>>> {
        self.inner.upgrade().unwrap()
    }

    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.inner, &other.inner)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HeapAllocated<T: Clone + std::fmt::Debug + PartialEq + Eq> {
    pub(crate) reachable: bool,
    pub(crate) value: T,
}

// Adding generation information should be doable here
// struct Test {
//     pub(crate) reachable: bool,
//     pub(crate) generation: u32,
//     pub(crate) value: SteelVal,
// }

#[test]
fn check_size_of_heap_allocated_value() {
    println!("{:?}", std::mem::size_of::<HeapAllocated<SteelVal>>());
    // println!("{:?}", std::mem::size_of::<Test>());
}

impl<T: Clone + std::fmt::Debug + PartialEq + Eq> HeapAllocated<T> {
    pub fn new(value: T) -> Self {
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

pub struct MarkAndSweepContext<'a> {
    // queue: &'a mut VecDeque<SteelVal>,
    queue: &'a mut Vec<SteelVal>,
}

impl<'a> MarkAndSweepContext<'a> {
    fn mark_heap_reference(&mut self, heap_ref: &Rc<RefCell<HeapAllocated<SteelVal>>>) {
        if heap_ref.borrow().is_reachable() {
            return;
        }

        {
            heap_ref.borrow_mut().mark_reachable();
        }

        self.push_back(heap_ref.borrow().value.clone());
    }

    // Visit the heap vector, mark it as visited!
    fn mark_heap_vector(&mut self, heap_vector: &Rc<RefCell<HeapAllocated<Vec<SteelVal>>>>) {
        if heap_vector.borrow().is_reachable() {
            return;
        }

        {
            heap_vector.borrow_mut().mark_reachable();
        }

        for value in heap_vector.borrow().value.iter() {
            self.push_back(value.clone());
        }
    }
}

impl<'a> BreadthFirstSearchSteelValVisitor for MarkAndSweepContext<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    fn pop_front(&mut self) -> Option<SteelVal> {
        // self.queue.pop_front()
        self.queue.pop()
    }

    fn push_back(&mut self, value: SteelVal) {
        match &value {
            SteelVal::BoolV(_)
            | SteelVal::NumV(_)
            | SteelVal::IntV(_)
            | SteelVal::CharV(_)
            | SteelVal::Void
            | SteelVal::StringV(_)
            | SteelVal::FuncV(_)
            | SteelVal::SymbolV(_)
            | SteelVal::FutureFunc(_)
            | SteelVal::FutureV(_)
            | SteelVal::BoxedFunction(_)
            | SteelVal::MutFunc(_)
            | SteelVal::BuiltIn(_)
            | SteelVal::BigNum(_) => return,
            _ => {
                // self.queue.push_back(value);
                self.queue.push(value);
            }
        }
    }

    fn visit_bignum(&mut self, _bignum: Gc<BigInt>) -> Self::Output {}

    fn visit_bool(&mut self, _boolean: bool) -> Self::Output {}
    fn visit_boxed_function(&mut self, _function: Rc<BoxedDynFunction>) -> Self::Output {}
    // TODO: Revisit this when the boxed iterator is cleaned up
    fn visit_boxed_iterator(&mut self, iterator: Gc<RefCell<OpaqueIterator>>) -> Self::Output {
        self.push_back(iterator.borrow().root.clone());
    }
    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) -> Self::Output {
        self.push_back(boxed_value.borrow().clone());
    }

    fn visit_builtin_function(&mut self, _function: BuiltInSignature) -> Self::Output {}

    fn visit_char(&mut self, _c: char) -> Self::Output {}
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
    fn visit_continuation(&mut self, continuation: Continuation) -> Self::Output {
        let continuation = (*continuation.inner.borrow()).clone();

        match continuation {
            ContinuationMark::Closed(continuation) => {
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

            ContinuationMark::Open(continuation) => {
                for value in &continuation.current_stack_values {
                    self.push_back(value.clone());
                }

                for value in &continuation.current_frame.function.captures {
                    self.push_back(value.clone());
                }
            }
        }
    }
    // TODO: Come back to this
    fn visit_custom_type(&mut self, custom_type: Gc<RefCell<Box<dyn CustomType>>>) -> Self::Output {
        custom_type.borrow().visit_children(self);
    }

    fn visit_float(&mut self, _float: f64) -> Self::Output {}

    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) -> Self::Output {}

    fn visit_future(&mut self, _future: Gc<FutureResult>) -> Self::Output {}

    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) -> Self::Output {}

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

    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        self.mark_heap_reference(&heap_ref.strong_ptr());
    }

    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        for value in vector.iter() {
            self.push_back(value.clone());
        }
    }
    fn visit_int(&mut self, _int: isize) -> Self::Output {}

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        for value in list {
            self.push_back(value);
        }
    }

    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) -> Self::Output {}

    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        self.mark_heap_vector(&vector.strong_ptr())
    }

    fn visit_port(&mut self, _port: Gc<SteelPort>) -> Self::Output {}

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

    // TODO: Revisit this
    fn visit_reference_value(&mut self, _reference: Rc<OpaqueReference<'static>>) -> Self::Output {}

    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output {
        for field in steel_struct.fields.iter() {
            self.push_back(field.clone());
        }
    }

    fn visit_stream(&mut self, stream: Gc<LazyStream>) -> Self::Output {
        self.push_back(stream.initial_value.clone());
        self.push_back(stream.stream_thunk.clone());
    }

    fn visit_string(&mut self, _string: SteelString) -> Self::Output {}

    fn visit_symbol(&mut self, _symbol: SteelString) -> Self::Output {}

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output {
        if let Some(raw) = syntax_object.raw.clone() {
            self.push_back(raw);
        }

        self.push_back(syntax_object.syntax.clone());
    }

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

    fn visit_void(&mut self) -> Self::Output {}
}
