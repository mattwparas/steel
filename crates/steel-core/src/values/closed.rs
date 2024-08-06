use std::{
    cell::RefCell,
    collections::HashSet,
    rc::{Rc, Weak},
};

use crate::{
    compiler::map::SymbolMap,
    rvals::{OpaqueIterator, SteelComplex, SteelVector},
    steel_vm::vm::{Continuation, ContinuationMark},
    values::lists::List,
};
use num::{BigInt, BigRational, Rational32};
use steel_gen::OpCode;

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

#[derive(Default)]
pub struct GlobalSlotRecycler {
    // Use a hashset to check for free slots.
    // The idea here is that a collection will traverse
    // all active values (excluding roots with this index)
    // and we'll check to make sure these are reachable.
    //
    // If the values are eventually reachable, then we can keep
    // iterating until this is completely drained.
    //
    // If we reach the end of our iteration and this isn't
    // drained, whatever is left is now freeable, and we can make
    // this as free in the symbol map
    slots: HashSet<usize>,

    queue: Vec<SteelVal>,
}

impl GlobalSlotRecycler {
    pub fn free_shadowed_rooted_values(
        roots: &mut Vec<SteelVal>,
        symbol_map: &mut SymbolMap,
        heap: &mut Heap,
    ) {
        let mut recycler = GlobalSlotRecycler::default();

        recycler.recycle(roots, symbol_map, heap);
    }

    // TODO:
    // Take the global roots, without the shadowed values, and iterate over them,
    // push the values back, visit, mark visited, move on.
    pub fn recycle(
        &mut self,
        roots: &mut Vec<SteelVal>,
        symbol_map: &mut SymbolMap,
        heap: &mut Heap,
    ) {
        self.slots.clear();

        // TODO: Right now, after one pass, we'll ignore it forever.
        // we should move it to another stage that we check later.
        for slot in symbol_map
            .free_list
            .shadowed_slots
            .drain(..)
            .chain(symbol_map.free_list.lambda_lifted.drain(..))
        {
            self.slots.insert(slot);
        }

        for (index, root) in roots.iter().enumerate() {
            if !self.slots.contains(&index) {
                self.push_back(root.clone());
            }
        }

        // Actually walk the tree, looking for unreachable stuff
        self.visit();

        // put them back as unreachable
        heap.memory.iter().for_each(|x| x.borrow_mut().reset());
        heap.vectors.iter().for_each(|x| x.borrow_mut().reset());

        // Anything that is still remaining will require
        // getting added to the free list that is left.
        for index in self.slots.drain() {
            if index < roots.len() {
                symbol_map.free_list.free_list.push(index);
                roots[index] = SteelVal::Void;
            }
        }
    }
}

impl<'a> BreadthFirstSearchSteelValVisitor for GlobalSlotRecycler {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    fn pop_front(&mut self) -> Option<SteelVal> {
        self.queue.pop()
    }

    fn visit(&mut self) -> Self::Output {
        use SteelVal::*;

        let mut ret = self.default_output();

        while let Some(value) = self.pop_front() {
            if self.slots.is_empty() {
                return;
            }

            ret = match value {
                Closure(c) => self.visit_closure(c),
                BoolV(b) => self.visit_bool(b),
                NumV(n) => self.visit_float(n),
                IntV(i) => self.visit_int(i),
                Rational(x) => self.visit_rational(x),
                BigRational(x) => self.visit_bigrational(x),
                BigNum(b) => self.visit_bignum(b),
                Complex(x) => self.visit_complex(x),
                CharV(c) => self.visit_char(c),
                VectorV(v) => self.visit_immutable_vector(v),
                Void => self.visit_void(),
                StringV(s) => self.visit_string(s),
                FuncV(f) => self.visit_function_pointer(f),
                SymbolV(s) => self.visit_symbol(s),
                SteelVal::Custom(c) => self.visit_custom_type(c),
                HashMapV(h) => self.visit_hash_map(h),
                HashSetV(s) => self.visit_hash_set(s),
                CustomStruct(c) => self.visit_steel_struct(c),
                PortV(p) => self.visit_port(p),
                IterV(t) => self.visit_transducer(t),
                ReducerV(r) => self.visit_reducer(r),
                FutureFunc(f) => self.visit_future_function(f),
                FutureV(f) => self.visit_future(f),
                StreamV(s) => self.visit_stream(s),
                BoxedFunction(b) => self.visit_boxed_function(b),
                ContinuationFunction(c) => self.visit_continuation(c),
                ListV(l) => self.visit_list(l),
                MutFunc(m) => self.visit_mutable_function(m),
                BuiltIn(b) => self.visit_builtin_function(b),
                MutableVector(b) => self.visit_mutable_vector(b),
                BoxedIterator(b) => self.visit_boxed_iterator(b),
                SteelVal::SyntaxObject(s) => self.visit_syntax_object(s),
                Boxed(b) => self.visit_boxed_value(b),
                Reference(r) => self.visit_reference_value(r),
                HeapAllocated(b) => self.visit_heap_allocated(b),
                Pair(p) => self.visit_pair(p),
                ByteVector(b) => self.visit_bytevector(b),
            };
        }

        ret
    }

    fn push_back(&mut self, value: SteelVal) {
        // TODO: Determine if all numbers should push back.
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
            | SteelVal::ByteVector(_)
            | SteelVal::BigNum(_) => return,
            _ => {
                self.queue.push(value);
            }
        }
    }

    fn visit_bytevector(&mut self, _bytevector: crate::rvals::SteelByteVector) -> Self::Output {}
    fn visit_bignum(&mut self, _: Gc<BigInt>) -> Self::Output {}
    fn visit_complex(&mut self, _: Gc<SteelComplex>) -> Self::Output {}
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
        // for heap_ref in closure.heap_allocated.borrow().iter() {
        // todo!()
        // self.mark_heap_reference(&heap_ref.strong_ptr())
        // }

        for capture in closure.captures() {
            self.push_back(capture.clone());
        }

        if let Some(contract) = closure.get_contract_information().as_ref() {
            self.push_back(contract.clone());
        }

        for instruction in closure.body_exp.iter() {
            match instruction.op_code {
                // If this instruction touches this global variable,
                // then we want to mark it as possibly referenced here.
                OpCode::CALLGLOBAL | OpCode::PUSH | OpCode::CALLGLOBALTAIL => {
                    self.slots.remove(&(instruction.payload_size.to_usize()));
                }
                _ => {}
            }
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

                    if let Some(handler) =
                        frame.attachments.as_ref().and_then(|x| x.handler.clone())
                    {
                        self.push_back(handler);
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
        let mut queue = MarkAndSweepContext {
            queue: &mut self.queue,
            object_count: 0,
        };

        custom_type.borrow().visit_children(&mut queue);
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
        let mut queue = MarkAndSweepContext {
            queue: &mut self.queue,
            object_count: 0,
        };

        queue.mark_heap_reference(&heap_ref.strong_ptr());
    }

    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        for value in vector.iter() {
            self.push_back(value.clone());
        }
    }
    fn visit_int(&mut self, _int: isize) -> Self::Output {}
    fn visit_rational(&mut self, _: Rational32) -> Self::Output {}
    fn visit_bigrational(&mut self, _: Gc<BigRational>) -> Self::Output {}

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        for value in list {
            self.push_back(value);
        }
    }

    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) -> Self::Output {}

    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        let mut queue = MarkAndSweepContext {
            queue: &mut self.queue,
            object_count: 0,
        };

        queue.mark_heap_vector(&vector.strong_ptr())
    }

    fn visit_port(&mut self, _port: SteelPort) -> Self::Output {}

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

    fn visit_pair(&mut self, pair: Gc<super::lists::Pair>) -> Self::Output {
        self.push_back(pair.car());
        self.push_back(pair.cdr());
    }
}

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

// TODO: If this proves to be faster, make these From(Vec<HeapValue>)
#[derive(Copy, Clone)]
enum CurrentSpace {
    From,
    To,
}

/// The heap for steel currently uses an allocation scheme based on weak references to reference counted pointers.
/// Allocation is just a `Vec<Rc<RefCell<T>>>`, where allocating simply pushes and allocates a value at the end.
/// When we do a collection, we attempt to do a small collection by just dropping any values with no weak counts
/// pointing to it.
#[derive(Clone)]
pub struct Heap {
    memory: Vec<HeapValue>,

    // from_space: Vec<HeapValue>,
    // to_space: Vec<HeapValue>,
    // current: CurrentSpace,
    vectors: Vec<HeapVector>,
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

            // from_space: Vec::with_capacity(256),
            // to_space: Vec::with_capacity(256),
            // current: CurrentSpace::From,
            vectors: Vec::with_capacity(256),
            count: 0,
            threshold: GC_THRESHOLD,
            // mark_and_sweep_queue: VecDeque::with_capacity(256),
            mark_and_sweep_queue: Vec::with_capacity(256),
            maybe_memory_size: 0,
        }
    }

    pub fn new_empty() -> Self {
        Heap {
            memory: Vec::new(),
            vectors: Vec::new(),
            count: 0,
            threshold: GC_THRESHOLD,
            mark_and_sweep_queue: Vec::new(),
            maybe_memory_size: 0,
        }
    }

    // #[inline(always)]
    // pub fn memory(&mut self) -> &mut Vec<HeapValue> {
    //     match self.current {
    //         CurrentSpace::From => &mut self.from_space,
    //         CurrentSpace::To => &mut self.to_space,
    //     }
    // }

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
        self.collect(
            Some(value.clone()),
            None,
            roots,
            live_functions,
            globals,
            false,
        );

        let pointer = Rc::new(RefCell::new(HeapAllocated::new(value)));
        let weak_ptr = Rc::downgrade(&pointer);

        self.memory.push(pointer);

        HeapRef { inner: weak_ptr }
    }

    pub fn allocate_without_collection<'a>(&mut self, value: SteelVal) -> HeapRef<SteelVal> {
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
        self.collect(None, Some(&values), roots, live_functions, globals, false);

        let pointer = Rc::new(RefCell::new(HeapAllocated::new(values)));
        let weak_ptr = Rc::downgrade(&pointer);

        self.vectors.push(pointer);

        HeapRef { inner: weak_ptr }
    }

    fn vector_cells_allocated(&self) -> usize {
        // self.vectors.iter().map(|x| x.borrow().value.len()).sum()
        self.vectors.len()
    }

    pub fn weak_collection(&mut self) {
        self.memory.retain(|x| Rc::weak_count(x) > 0);
        self.vectors.retain(|x| Rc::weak_count(x) > 0);
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
        force_full: bool,
    ) -> usize {
        let memory_size = self.memory.len() + self.vector_cells_allocated();

        if memory_size > self.threshold || force_full {
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
            let mut i = 0;
            while changed && i < 3 {
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
                i += 1;
            }

            let post_small_collection_size = self.memory.len() + self.vector_cells_allocated();

            let mut amount = 0;

            // Mark + Sweep!
            if post_small_collection_size as f64 > (0.25 * original_length as f64) || force_full {
                log::debug!(target: "gc", "---- Post small collection, running mark and sweep - heap size filled: {:?} ----", post_small_collection_size as f64 / original_length as f64);

                amount =
                    self.mark_and_sweep(root_value, root_vector, roots, live_functions, globals);
            } else {
                log::debug!(target: "gc", "---- Skipping mark and sweep - heap size filled: {:?} ----", post_small_collection_size as f64 / original_length as f64);
            }

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

            return amount;
        }

        0
    }

    fn mark_and_sweep<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: Option<&Vec<SteelVal>>,
        roots: impl Iterator<Item = &'a SteelVal>,
        function_stack: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: impl Iterator<Item = &'a SteelVal>,
    ) -> usize {
        log::debug!(target: "gc", "Marking the heap");

        #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        let mut context = MarkAndSweepContext {
            queue: &mut self.mark_and_sweep_queue,
            object_count: 0,
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
            // for heap_ref in function.heap_allocated.borrow().iter() {
            //     context.mark_heap_reference(&heap_ref.strong_ptr())
            // }

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

        #[cfg(feature = "profiling")]
        log::debug!(target: "gc", "Mark: Time taken: {:?}", now.elapsed());

        #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        let object_count = context.object_count;

        log::debug!(target: "gc", "--- Sweeping ---");
        let prior_len = self.memory.len() + self.vector_cells_allocated();

        // sweep
        self.memory.retain(|x| x.borrow().is_reachable());
        self.vectors.retain(|x| x.borrow().is_reachable());

        let after_len = self.memory.len();

        let amount_freed = prior_len - after_len;

        log::debug!(target: "gc", "Freed objects: {:?}", amount_freed);
        log::debug!(target: "gc", "Objects alive: {:?}", after_len);

        // put them back as unreachable
        self.memory.iter().for_each(|x| x.borrow_mut().reset());
        self.vectors.iter().for_each(|x| x.borrow_mut().reset());

        ROOTS.with(|x| x.borrow_mut().increment_generation());

        #[cfg(feature = "profiling")]
        log::debug!(target: "gc", "Sweep: Time taken: {:?}", now.elapsed());

        object_count.saturating_sub(amount_freed)
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
    queue: &'a mut Vec<SteelVal>,
    object_count: usize,
}

impl<'a> MarkAndSweepContext<'a> {
    pub(crate) fn mark_heap_reference(&mut self, heap_ref: &Rc<RefCell<HeapAllocated<SteelVal>>>) {
        if heap_ref.borrow().is_reachable() {
            return;
        }

        {
            heap_ref.borrow_mut().mark_reachable();
        }

        self.push_back(heap_ref.borrow().value.clone());
    }

    // Visit the heap vector, mark it as visited!
    pub(crate) fn mark_heap_vector(
        &mut self,
        heap_vector: &Rc<RefCell<HeapAllocated<Vec<SteelVal>>>>,
    ) {
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
        self.queue.pop()
    }

    fn push_back(&mut self, value: SteelVal) {
        self.object_count += 1;

        // TODO: Determine if all numbers should push back.
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
            | SteelVal::ByteVector(_)
            | SteelVal::BigNum(_) => return,
            _ => {
                self.queue.push(value);
            }
        }
    }

    fn visit_bytevector(&mut self, _bytevector: crate::rvals::SteelByteVector) -> Self::Output {}
    fn visit_bignum(&mut self, _: Gc<BigInt>) -> Self::Output {}
    fn visit_complex(&mut self, _: Gc<SteelComplex>) -> Self::Output {}
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
        // for heap_ref in closure.heap_allocated.borrow().iter() {
        //     self.mark_heap_reference(&heap_ref.strong_ptr())
        // }

        for capture in closure.captures() {
            self.push_back(capture.clone());
        }

        if let Some(contract) = closure.get_contract_information().as_ref() {
            self.push_back(contract.clone());
        }
    }
    fn visit_continuation(&mut self, continuation: Continuation) -> Self::Output {
        // TODO: Don't clone this here!
        let continuation = (*continuation.inner.borrow()).clone();

        match continuation {
            ContinuationMark::Closed(continuation) => {
                for value in continuation.stack {
                    self.push_back(value);
                }

                for value in &continuation.current_frame.function.captures {
                    self.push_back(value.clone());
                }

                for frame in continuation.stack_frames {
                    for value in &frame.function.captures {
                        self.push_back(value.clone());
                    }

                    if let Some(handler) =
                        frame.attachments.as_ref().and_then(|x| x.handler.clone())
                    {
                        self.push_back(handler);
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
    fn visit_rational(&mut self, _: Rational32) -> Self::Output {}
    fn visit_bigrational(&mut self, _: Gc<BigRational>) -> Self::Output {}

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        for value in list {
            self.push_back(value);
        }
    }

    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) -> Self::Output {}

    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        self.mark_heap_vector(&vector.strong_ptr())
    }

    fn visit_port(&mut self, _port: SteelPort) -> Self::Output {}

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

    fn visit_pair(&mut self, pair: Gc<super::lists::Pair>) -> Self::Output {
        self.push_back(pair.car());
        self.push_back(pair.cdr());
    }
}
