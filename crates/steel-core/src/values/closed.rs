use std::{cell::RefCell, collections::HashSet, sync::Arc, thread::JoinHandle};

#[cfg(feature = "sync")]
use std::sync::Mutex;

use crate::{
    compiler::map::SymbolMap,
    gc::{
        shared::{MutContainer, ShareableMut, StandardShared, StandardSharedMut, WeakShared},
        GcMut,
    },
    rvals::{
        cycles::BreadthFirstSearchSteelValReferenceVisitor2, AsRefSteelVal, Custom, IntoSteelVal,
        OpaqueIterator, RestArgsIter, SteelComplex, SteelValPointer, SteelVector,
    },
    steel_vm::vm::{apply, Continuation, ContinuationMark, Synchronizer, VmCore},
    values::lists::List,
    SteelErr,
};
use crossbeam_channel::{Receiver, Sender};
use num_bigint::BigInt;
use num_rational::{BigRational, Rational32};

#[cfg(feature = "sync")]
use once_cell::sync::Lazy;

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
    lists::Pair,
    port::SteelPort,
    structs::UserDefinedStruct,
    transducers::{Reducer, Transducer},
    Vector,
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
        roots: &mut [SteelVal],
        symbol_map: &mut SymbolMap,
        heap: &mut Heap,
    ) {
        let mut recycler = GlobalSlotRecycler::default();

        recycler.recycle(roots, symbol_map, heap);
    }

    // TODO:
    // Take the global roots, without the shadowed values, and iterate over them,
    // push the values back, visit, mark visited, move on.
    pub fn recycle(&mut self, roots: &mut [SteelVal], symbol_map: &mut SymbolMap, heap: &mut Heap) {
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

        // Mark all unreachable for the purposes of the global
        // collection.
        // heap.memory_free_list.mark_all_unreachable();
        // heap.vector_free_list.mark_all_unreachable();

        // Actually walk the tree, looking for unreachable stuff
        self.visit();

        // heap.memory_free_list.recount();
        // heap.vector_free_list.recount();

        // TODO: Check this stuff!
        // put them back as unreachable

        // heap.memory.iter().for_each(|x| x.write().reset());
        // heap.vectors.iter().for_each(|x| x.write().reset());

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

        while let Some(value) = self.pop_front() {
            if self.slots.is_empty() {
                return;
            }

            match value {
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
            | SteelVal::BigNum(_) => {}
            _ => {
                self.queue.push(value);
            }
        }
    }

    fn visit_bytevector(&mut self, _bytevector: crate::rvals::SteelByteVector) -> Self::Output {}
    fn visit_bignum(&mut self, _: Gc<BigInt>) -> Self::Output {}
    fn visit_complex(&mut self, _: Gc<SteelComplex>) -> Self::Output {}
    fn visit_bool(&mut self, _boolean: bool) -> Self::Output {}
    fn visit_boxed_function(&mut self, _function: Gc<BoxedDynFunction>) -> Self::Output {}
    // TODO: Revisit this when the boxed iterator is cleaned up
    fn visit_boxed_iterator(&mut self, iterator: GcMut<OpaqueIterator>) -> Self::Output {
        self.push_back(iterator.read().root.clone());
    }
    fn visit_boxed_value(&mut self, boxed_value: GcMut<SteelVal>) -> Self::Output {
        self.push_back(boxed_value.read().clone());
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
        let continuation = (*continuation.inner.read()).clone();

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

                    // if let Some(handler) = &frame.handler {
                    //     self.push_back((*handler.as_ref()).clone());
                    // }

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
    fn visit_custom_type(&mut self, custom_type: GcMut<Box<dyn CustomType>>) -> Self::Output {
        let mut queue = MarkAndSweepContext {
            queue: &mut self.queue,
            object_count: 0,
        };

        custom_type.read().visit_children(&mut queue);
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
    fn visit_reference_value(&mut self, _reference: Gc<OpaqueReference<'static>>) -> Self::Output {}

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

// TODO: Do these roots needs to be truly global?
// Replace this with a lazy static
thread_local! {
    static ROOTS: RefCell<Roots> = RefCell::new(Roots::default());
}

// stash roots in the global area
#[cfg(feature = "sync")]
static GLOBAL_ROOTS: Lazy<Mutex<Roots>> = Lazy::new(|| Mutex::new(Roots::default()));

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
    #[cfg(not(feature = "sync"))]
    fn drop(&mut self) {
        ROOTS.with(|x| x.borrow_mut().free(self))
    }

    #[cfg(feature = "sync")]
    fn drop(&mut self) {
        GLOBAL_ROOTS.lock().unwrap().free(self)
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
        #[cfg(feature = "sync")]
        {
            GLOBAL_ROOTS.lock().unwrap().root(self.clone())
        }

        #[cfg(not(feature = "sync"))]
        {
            ROOTS.with(|x| x.borrow_mut().root(self.clone()))
        }
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

// type HeapValue = SharedMut<HeapAllocated<SteelVal>>;
// type HeapVector = SharedMut<HeapAllocated<Vec<SteelVal>>>;

type HeapValue = StandardSharedMut<HeapAllocated<SteelVal>>;
type HeapVector = StandardSharedMut<HeapAllocated<Vec<SteelVal>>>;

type HeapElement<T> = StandardSharedMut<HeapAllocated<T>>;

#[derive(Copy, Clone)]
enum Reachable {
    // This is legal to allocate from
    Dead,

    // Definitely unreachable. These are legal to be allocated from
    Black,
    // Maybe reachable. Things that survive a collection
    Grey,
    // Reachable. Post collection, we want to reset things to be reachable.
    White,
}

static MARKER: std::sync::LazyLock<ParallelMarker> = std::sync::LazyLock::new(ParallelMarker::new);

// This can get pretty big. Don't want to have to re allocate every time it grows.
// "Growing" in this case is going to be putting a new block on to the back of the
// pointers. Yes, we'll do some pointer chasing, but it should be okay.
struct MemoryBlocks<T: HeapAble> {
    current_block: usize,

    // We should always have one pinned memory block. We probably can use like
    // Arc<[Foo]> since we don't actually need that capacity. Dropping can just be popping
    // off blocks that we don't need anymore.
    blocks: Vec<Vec<HeapElement<T>>>,
}

impl<T: HeapAble> MemoryBlocks<T> {
    pub fn new() -> Self {
        Self {
            current_block: 0,
            blocks: Vec::new(),
        }
    }

    pub fn grow(&mut self) {
        let current_block = &self.blocks[self.current_block];
        let size = current_block.len();
        let new_block = 2 * size;

        // Append new block.
        self.blocks.push(
            std::iter::repeat_with(|| {
                StandardShared::new(MutContainer::new(HeapAllocated::new(T::empty())))
            })
            .take(new_block)
            .collect(),
        );
    }
}

// Allocate a box, and put it in here
// to be marked as reachable?
pub struct WillExecutor {
    // Determining if these are reachable should be either:
    // * Value is a weak reference: then you use weak references
    // * value is contained within _other_ will executors, so the will executors need to have references to
    //   each other to check if ther value is contained within other ones.
    values: Mutex<Vec<Option<Pair>>>,

    // The incoming queue. We'll drain on this when the reified buffer of values
    // is empty since we need to scan the values to find the right one.
    incoming: Receiver<Pair>,

    sender: Sender<Pair>,
}

// Check the strong counts. If the strong count is 1 for a value if its a GC pointer,
// then we can can drop it and call the thunk. It should probably get placed into a weak
// box?
impl WillExecutor {
    fn block_until_incoming(&self) {
        log::debug!(target: "will", "Blocking until something shows in the queue");
        if let Ok(next) = self.incoming.recv() {
            self.values.lock().unwrap().push(Some(next));
        }
    }

    // Returns the pair of values next to be used. Not the best data structure choice.
    fn find_next(&self) -> Option<Pair> {
        log::debug!(target: "will", "Finding the next value in the will executor list");
        // Proactively check if the value is ready to be received.
        // TODO: Merge the behavior below and above since we can just return this value
        // immediately without putting on to the queue.
        let top_guard = self.values.lock().unwrap();
        if top_guard.is_empty() && self.incoming.is_empty() {
            log::debug!(target: "will", "Initial values list is empty. Waiting on the queue.");
            drop(top_guard);

            // Block until the next thing is pushed on
            if let Ok(next) = self.incoming.recv() {
                log::debug!(target: "will", "Found a value off the queue, trying to lock the values.");

                self.values.lock().unwrap().push(Some(next));
            }
        } else {
            drop(top_guard);
        }

        log::debug!(target: "will", "Done optimistically checking off the queue. Locking the values.");

        let mut guard = self.values.lock().unwrap();

        // Drain the incoming, push them in to the values:

        while let Ok(incoming) = self.incoming.try_recv() {
            // TODO: Use a free list here instead? That way we can
            // re-use the space provided from earlier? Run some compaction
            // every one in a while?
            guard.push(Some(incoming));
        }

        log::debug!(target: "will", "Iterating over the values to drop");

        // TODO:
        // Periodically run a compaction on this
        for pair_slot in guard.iter_mut() {
            if let Some(value) = pair_slot {
                let is_sole_reference = match value.car_ref() {
                    SteelVal::Closure(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::VectorV(steel_vector) => Gc::strong_count(&steel_vector.0) == 1,
                    SteelVal::StringV(steel_string) => Gc::strong_count(&steel_string.0) == 1,
                    SteelVal::SymbolV(steel_string) => Gc::strong_count(&steel_string.0) == 1,
                    SteelVal::Custom(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::HashMapV(steel_hash_map) => Gc::strong_count(&steel_hash_map.0) == 1,
                    SteelVal::HashSetV(steel_hash_set) => Gc::strong_count(&steel_hash_set.0) == 1,
                    SteelVal::CustomStruct(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::PortV(steel_port) => Gc::strong_count(&steel_port.port) == 1,
                    SteelVal::IterV(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::ReducerV(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::FutureV(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::StreamV(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::BoxedFunction(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::ContinuationFunction(continuation) => {
                        StandardShared::strong_count(&continuation.inner) == 1
                    }
                    SteelVal::ListV(generic_list) => generic_list.strong_count() == 1,
                    SteelVal::Pair(gc) => Gc::strong_count(gc) == 1,

                    // In theory, if this is a weak value, this can be the thing to check
                    // the values.
                    SteelVal::MutableVector(heap_ref) => {
                        WeakShared::weak_count(&heap_ref.inner) == 1
                            || !heap_ref.inner.upgrade().unwrap().read().is_reachable()
                    }
                    SteelVal::BoxedIterator(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::SyntaxObject(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::Boxed(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::HeapAllocated(heap_ref) => {
                        WeakShared::weak_count(&heap_ref.inner) == 1
                            || !heap_ref.inner.upgrade().unwrap().read().is_reachable()
                    }
                    SteelVal::Reference(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::BigNum(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::BigRational(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::Complex(gc) => Gc::strong_count(gc) == 1,
                    SteelVal::ByteVector(steel_byte_vector) => {
                        Gc::strong_count(&steel_byte_vector.vec) == 1
                    }
                    _ => false,
                };

                if is_sole_reference {
                    let value = std::mem::take(pair_slot).unwrap();
                    return Some(value);
                }
            }
        }

        // Compact the values
        guard.retain(|x| x.is_some());

        None
    }

    pub fn register(&self, key: SteelVal, func: SteelVal) -> Result<(), SteelErr> {
        log::debug!(target: "will", "Registering: {}", key);
        if !func.is_function() {
            stop!(TypeMismatch => "will-register expects a function, found: {}", func)
        }

        self.sender
            .send(Pair {
                car: key,
                cdr: func,
            })
            .unwrap();

        Ok(())
    }
}

// Doesn't reach the children values?
impl Custom for WillExecutor {}

#[steel_derive::function(name = "make-will-executor")]
pub fn make_will_executor() -> Result<SteelVal, SteelErr> {
    let (sender, incoming) = crossbeam_channel::unbounded();
    WillExecutor {
        values: Mutex::new(Vec::new()),
        sender,
        incoming,
    }
    .into_steelval()
}

#[steel_derive::context(name = "will-execute", arity = "Exact(1)")]
pub fn will_execute(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal, SteelErr>> {
    let executor = WillExecutor::as_ref(&args[0]).unwrap();

    // TODO: Make this find the next thing? Perhaps just have this be a channel
    // or something that waits until the next thing is found?
    // How to do this _with_ blocking? Maybe have a queue for values that are being registered,
    // and then otherwise drain the vector and put them in when found.
    if let SteelVal::Pair(pair) = ctx
        .thread
        .enter_safepoint(|_| loop {
            if let Some(value) = executor.find_next() {
                return Ok(SteelVal::Pair(Gc::new(value)));
            } else {
                // Control frequency here, but for when there is no throughput,
                // we want to at least poll a bit
                executor.block_until_incoming();
            }
        })
        .unwrap()
    {
        let Pair { car, cdr } = pair.try_unwrap().unwrap();
        apply(ctx, &[cdr, SteelVal::ListV(vec![car].into())])
    } else {
        unreachable!()
    }
}

#[steel_derive::function(name = "will-register")]
pub fn will_register(
    will: SteelVal,
    value: SteelVal,
    func: SteelVal,
) -> Result<SteelVal, SteelErr> {
    let will = WillExecutor::as_ref(&will)?;
    will.register(value, func)?;
    Ok(SteelVal::Void)
}

// Not considered part of the reachability graph? i.e. we simply don't traverse it.
pub struct WeakBox {
    value: HeapRef<SteelVal>,
}

impl Custom for WeakBox {}

/// Allocates a weak box.
///
/// A weak box is similar to a box, but when the garbage collector can prove
/// that the value of a weak box is only reachable through weak references,
/// the weak box value will always return #false.
///
/// In other words, a weak box does not keep the value contained alive through
/// a gc collection.
#[steel_derive::context(name = "make-weak-box", arity = "Exact(1)")]
pub fn make_weak_box(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal, SteelErr>> {
    let value = args[0].clone();
    if let SteelVal::HeapAllocated(made_box) = ctx.make_box(value) {
        Some(WeakBox { value: made_box }.into_steelval())
    } else {
        unreachable!()
    }
}

/// Returns the value contained in the weak box.
/// If the garbage collector has proven that the previous content
/// value of weak-box was reachable only through a weak reference,
/// then default-value (which defaults to #f) is returned.
///
/// ```scheme
/// (define value (make-weak-box 10))
/// (weak-box-value value) ;; => 10
/// (set! value #f) ;; Wipe out the previous value
/// (#%gc-collect)
/// (weak-box-value value) ;; => #false
/// ```
#[steel_derive::function(name = "weak-box-value")]
pub fn weak_box_value(
    value: &SteelVal,
    mut rest: RestArgsIter<&SteelVal>,
) -> Result<SteelVal, SteelErr> {
    let inner = WeakBox::as_ref(value)?;

    // Check if its reachable?
    if let Some(value) = inner.value.maybe_get_from_weak() {
        Ok(value)
    } else {
        Ok(rest
            .next()
            .map(|x| x.unwrap().clone())
            .unwrap_or(SteelVal::BoolV(false)))
    }
}

// Have free list for vectors and values separately. Can keep some of the vectors pre allocated
// as well, as necessary.
//
// Also -> implement will / executor functionality for implementing destructors for objects.
// This should be _relatively_ straightforward. When allocating a value, send it on the destructor
// thread.
#[derive(Debug)]
struct FreeList<T: HeapAble> {
    // TODO: Make this a linked list of vectors. Can reuse im-lists?
    // Pointer chasing may not be worth it. The issue is every doubling of size _also_
    // allocate a whole bunch more. Can probably just get away with a vec of vecs to avoid
    // copying everything.
    elements: Vec<HeapElement<T>>,
    cursor: usize,

    // Available count
    alloc_count: usize,

    grow_count: usize,
    forward: Option<Sender<Vec<HeapElement<T>>>>,
    backward: Option<Receiver<Vec<HeapElement<T>>>>,
}

impl<T: HeapAble> Clone for FreeList<T> {
    fn clone(&self) -> Self {
        Self {
            cursor: self.cursor,
            alloc_count: self.alloc_count,
            grow_count: self.grow_count,
            elements: self
                .elements
                .iter()
                .map(|x| {
                    let guard = x.read();
                    let inner = guard.value.clone();
                    StandardShared::new(MutContainer::new(HeapAllocated {
                        reachable: guard.reachable,
                        finalizer: guard.finalizer,
                        value: inner,
                    }))
                })
                .collect(),
            forward: None,
            backward: None,
        }
    }
}

#[test]
fn basic_free_list_usage() {
    // Pre allocate some slots
    let mut free_list: FreeList<SteelVal> = FreeList::new();

    let pointers = (0..100)
        .into_iter()
        .map(|x| free_list.allocate(SteelVal::IntV(x)))
        .collect::<Vec<_>>();

    drop(pointers);

    free_list.weak_collection();

    for var in &free_list.elements {
        assert!(!var.read().is_reachable());
    }
}

#[test]
fn free_list_continues_allocating_when_full() {
    // Pre allocate some slots
    let mut free_list: FreeList<SteelVal> = FreeList::new();

    let count: usize = 10000;

    let pointers = (0..count)
        .into_iter()
        .map(|x| free_list.allocate(SteelVal::IntV(x as isize)))
        .collect::<Vec<_>>();

    for var in &free_list.elements[0..count] {
        assert!(var.read().is_reachable());
    }

    for var in &free_list.elements[count..] {
        assert!(!var.read().is_reachable());
    }

    drop(pointers);

    free_list.weak_collection();

    for var in &free_list.elements {
        assert!(!var.read().is_reachable());
    }
}

#[test]
fn free_list_continues_allocating_in_the_middle() {
    // Pre allocate some slots
    let mut free_list: FreeList<SteelVal> = FreeList::new();

    let count: usize = 10000;

    let mut pointers = (0..count)
        .into_iter()
        .map(|x| free_list.allocate(SteelVal::IntV(x as isize)))
        .collect::<Vec<_>>();

    for var in &free_list.elements[0..count] {
        assert!(var.read().is_reachable());
    }

    for var in &free_list.elements[count..] {
        assert!(!var.read().is_reachable());
    }

    let right_half = pointers.split_off(100);

    drop(pointers);

    free_list.weak_collection();

    // Check that the first 100 elements are in fact, gone

    for var in &free_list.elements[0..100] {
        assert!(!var.read().is_reachable());
    }

    drop(right_half)
}

impl<T: HeapAble + Sync + Send + 'static> FreeList<T> {
    // TODO: Calculate the overhead!
    // How big is this?
    const EXTEND_CHUNK: usize = 256 * 1000;

    fn new() -> Self {
        let (forward_sender, backward_receiver) = spawn_background_dropper();

        let mut res = FreeList {
            elements: Vec::new(),
            cursor: 0,
            alloc_count: 0,
            grow_count: 0,
            forward: Some(forward_sender),
            backward: Some(backward_receiver),
        };

        res.grow();

        res
    }

    // Send the other block to the background for compaction
    // fn swap_blocks(&mut self) {
    //     let memory = std::mem::take(&mut self.elements);
    //     self.forward.send(memory).unwrap();
    //     self.elements = self.backward.recv().unwrap();

    //     self.alloc_count = 0;
    //     self.grow_count = 0;
    //     self.extend_heap();
    // }

    // Update the counts for things
    fn recount(&mut self) {
        self.alloc_count = 0;
        for element in &mut self.elements {
            let guard = element.read();
            if !guard.is_reachable() {
                // Replace with an empty value... for now.
                // Want to keep the memory counts down.
                // let value = std::mem::replace(&mut guard.value, T::empty());

                self.alloc_count += 1;
            }
        }
    }

    fn percent_full(&self) -> f64 {
        let count = self.elements.len() as f64;

        let percent = (count - self.alloc_count as f64) / count;

        assert!(percent < 1.01);

        percent
    }

    fn is_heap_full(&self) -> bool {
        self.alloc_count == 0
    }

    fn grow(&mut self) {
        let now = std::time::Instant::now();
        // Can probably make this a lot bigger
        let current = self.elements.len().max(Self::EXTEND_CHUNK);

        self.cursor = self.elements.len();

        self.elements.reserve(current);

        log::debug!(target: "gc", "Time to extend the heap vec -> {:?}", now.elapsed());

        let now = std::time::Instant::now();

        // Can we pre allocate this somewhere else? Incrementally allocate the values?
        // So basically we can have the elements be allocated vs not, and just have them
        // be snatched on demand?
        self.elements.extend(
            std::iter::repeat_with(|| {
                StandardShared::new(MutContainer::new(HeapAllocated::new(T::empty())))
            })
            .take(current),
        );

        log::debug!(target: "gc", "Growing the heap by: {} -> {:?}", current, now.elapsed());

        self.alloc_count += current;
        self.grow_count += 1;

        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].read().is_reachable());
        }
    }

    // Extend the heap
    fn extend_heap(&mut self) {
        self.grow();

        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].read().is_reachable());
        }
    }

    // if it points to another thing, consider marking it as unreachable?
    fn seek_to_next_free(&mut self) {
        todo!()
    }

    fn allocate(&mut self, value: T) -> HeapRef<T> {
        // Drain, moving values around...
        // is that expensive?
        let guard = &mut self.elements[self.cursor];

        // Allocate into this field
        let mut heap_guard = guard.write();

        // TODO: If the guard is registered with a will executor,
        // then we shouldn't mark this value as eligible to be
        // freed?
        heap_guard.value = value;

        heap_guard.reachable = true;
        let weak_ptr = StandardShared::downgrade(&guard);
        drop(heap_guard);

        // self.elements[self.cursor] = pointer;
        self.alloc_count -= 1;

        // Find where to assign the next slot optimistically?
        let next_slot = self.elements[self.cursor..]
            .iter()
            .position(|x| !x.read().is_reachable());

        if let Some(next_slot) = next_slot {
            self.cursor += next_slot;

            // #[cfg(debug_assertions)]
            // {
            // assert!(!self.elements[self.cursor].read().is_reachable());
            // }
        } else {
            // TODO: Handle compaction and moving things around so the
            // cursor has a chance to actually find stuff that has been
            // freed. It would also be nice
            if self.is_heap_full() {
                log::debug!(target: "gc", "Extending the heap in `allocate`");

                // Extend the heap, move the cursor to the end
                self.extend_heap();

                // assert!(!self.elements[self.cursor].read().is_reachable());
            } else {
                // Move to the beginning.
                self.cursor = self
                    .elements
                    .iter()
                    .position(|x| !x.read().is_reachable())
                    .unwrap();

                // assert!(!self.elements[self.cursor].read().is_reachable());
            }
        }

        // assert!(!self.elements[self.cursor].read().is_reachable());

        HeapRef { inner: weak_ptr }
    }

    // Can incrementally collect with the from / to space, assuming that the collections
    // are done incrementally from the other side.
    fn collect_on_condition(&mut self, func: fn(&HeapElement<T>) -> bool) -> usize {
        log::debug!(target: "gc", "Free count before weak collection: {}", self.alloc_count);
        let mut amount_dropped = 0;

        self.elements.iter_mut().for_each(|x| {
            // This is... a little gnarly? We don't want to lock each time, but it could
            // help. Allocations can now be genuinely reused since we're manipulating
            // what is inside the pointer
            if func(x) {
                let mut guard = x.write();

                if guard.reachable {
                    guard.reachable = false;
                    amount_dropped += 1;
                }
            }
        });

        self.alloc_count += amount_dropped;
        log::debug!(target: "gc", "Free count after weak collection: {}", self.alloc_count);

        amount_dropped
    }

    // Full weak collection
    fn weak_collection(&mut self) -> usize {
        // Just mark them to be dead
        let res = self.collect_on_condition(|inner| StandardShared::weak_count(inner) == 0);
        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].read().is_reachable());
        }
        res
    }

    fn mark_all_unreachable(&mut self) {
        self.elements.iter_mut().for_each(|x| x.write().reset());
    }

    // Compact every once in a while
    // TODO: Move this on to its own thread
    fn compact(&mut self) {
        // self.elements.retain(|x| x.read().is_reachable());
        // self.elements.shrink_to_fit();

        if let Some(sender) = &self.forward {
            sender.send(std::mem::take(&mut self.elements)).unwrap();
            self.elements = self.backward.as_ref().unwrap().recv().unwrap();
        }

        log::debug!(target: "gc", "Heap size after compaction: {}", self.elements.len());
        self.alloc_count = 0;
        self.grow_count = 0;
        self.extend_heap();
        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].read().is_reachable());
        }
    }

    fn strong_collection(&mut self) -> usize {
        self.collect_on_condition(|inner| !inner.read().is_reachable())
    }
}

impl FreeList<Vec<SteelVal>> {
    fn allocate_vec(&mut self, value: impl Iterator<Item = SteelVal>) -> HeapRef<Vec<SteelVal>> {
        // Drain, moving values around...
        // is that expensive?
        let guard = &mut self.elements[self.cursor];

        // Allocate into this field
        let mut heap_guard = guard.write();

        // Check that this fits:
        heap_guard.value.clear();
        for v in value {
            heap_guard.value.push(v);
        }

        heap_guard.value.shrink_to_fit();

        heap_guard.reachable = true;
        let weak_ptr = StandardShared::downgrade(&guard);
        drop(heap_guard);

        // self.elements[self.cursor] = pointer;
        self.alloc_count -= 1;

        // Find where to assign the next slot optimistically?
        let next_slot = self.elements[self.cursor..]
            .iter()
            .position(|x| !x.read().is_reachable());

        if let Some(next_slot) = next_slot {
            self.cursor += next_slot;

            // #[cfg(debug_assertions)]
            // {
            // assert!(!self.elements[self.cursor].read().is_reachable());
            // }
        } else {
            // TODO: Handle compaction and moving things around so the
            // cursor has a chance to actually find stuff that has been
            // freed. It would also be nice
            if self.is_heap_full() {
                log::debug!(target: "gc", "Extending the heap in `allocate`");

                // Extend the heap, move the cursor to the end
                self.extend_heap();

                // assert!(!self.elements[self.cursor].read().is_reachable());
            } else {
                // Move to the beginning.
                self.cursor = self
                    .elements
                    .iter()
                    .position(|x| !x.read().is_reachable())
                    .unwrap();

                // assert!(!self.elements[self.cursor].read().is_reachable());
            }
        }

        // assert!(!self.elements[self.cursor].read().is_reachable());

        HeapRef { inner: weak_ptr }
    }
}

// Try out the background dropper as a way to eliminate big pauses
fn spawn_background_dropper<T: HeapAble + Sync + Send + 'static>(
) -> (Sender<Vec<HeapElement<T>>>, Receiver<Vec<HeapElement<T>>>) {
    let (forward_sender, forward_receiver) = crossbeam_channel::bounded(0);
    let (backward_sender, backward_receiver) = crossbeam_channel::bounded(0);

    // Worker thread, capable of dropping the background values of lists
    std::thread::spawn(move || {
        let mut current: Vec<HeapElement<T>> = Vec::new();
        for mut block in forward_receiver {
            std::mem::swap(&mut current, &mut block);
            // Get the value, move on.
            backward_sender.send(block).unwrap();

            // TODO: when compacting, keep values that have been registered with a will
            // executor.
            current.retain(|x| x.read().is_reachable());
            current.shrink_to_fit();
        }
    });
    (forward_sender, backward_receiver)
}

// TODO: If this proves to be faster, make these From(Vec<HeapValue>)
#[derive(Copy, Clone)]
enum CurrentSpace {
    From,
    To,
}

/// The heap for steel currently uses an allocation scheme based on weak references
/// to reference counted pointers. Allocation is just a `Vec<Rc<RefCell<T>>>`, where
/// allocating simply pushes and allocates a value at the end. When we do a collection,
/// we attempt to do a small collection by just dropping any values with no weak counts
/// pointing to it.
#[derive(Clone)]
pub struct Heap {
    memory: Vec<HeapValue>,

    vectors: Vec<HeapVector>,
    count: usize,
    threshold: usize,
    mark_and_sweep_queue: Vec<SteelVal>,

    test_queue: Vec<*const SteelVal>,

    maybe_memory_size: usize,

    skip_minor_collection: bool,
    memory_free_list: FreeList<SteelVal>,
    vector_free_list: FreeList<Vec<SteelVal>>,
}

unsafe impl Send for Heap {}
unsafe impl Sync for Heap {}

// Contiguous... no good? Perhaps a free list is actually better here?
// Can reuse the allocations more effectively, and can compact where needed.
struct MemorySpace {
    memory: Vec<HeapValue>,
    vectors: Vec<HeapVector>,
}

type MemoryBlock = (Vec<HeapValue>, Vec<HeapVector>);

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

            test_queue: Vec::with_capacity(256),

            maybe_memory_size: 0,

            skip_minor_collection: false,

            memory_free_list: FreeList::new(),
            vector_free_list: FreeList::new(),
        }
    }

    pub fn new_empty() -> Self {
        Heap {
            memory: Vec::new(),
            vectors: Vec::new(),
            count: 0,
            threshold: GC_THRESHOLD,
            mark_and_sweep_queue: Vec::new(),
            test_queue: Vec::new(),
            maybe_memory_size: 0,
            skip_minor_collection: false,
            memory_free_list: FreeList::new(),
            vector_free_list: FreeList::new(),
        }
    }

    // TODO: Get rid of this
    pub fn allocate_without_collection<'a>(&mut self, value: SteelVal) -> HeapRef<SteelVal> {
        let pointer = StandardShared::new(MutContainer::new(HeapAllocated::new(value)));
        let weak_ptr = StandardShared::downgrade(&pointer);

        self.memory.push(pointer);

        HeapRef { inner: weak_ptr }
    }

    fn vector_cells_allocated(&self) -> usize {
        // self.vectors.iter().map(|x| x.borrow().value.len()).sum()
        self.vectors.len()
    }

    pub fn weak_collection(&mut self) {
        self.memory.retain(|x| StandardShared::weak_count(x) > 0);
        self.vectors.retain(|x| StandardShared::weak_count(x) > 0);
    }

    // TODO: Call this in more areas in the VM to attempt to free memory more carefully
    // Also - come up with generational scheme if possible
    pub fn collect<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: impl Iterator<Item = SteelVal>,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
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

            // From the prior run. If we made no progress from a minor collection,
            // we should simply skip it this time. Since its likely that we won't
            // continue to make progress with running a minor collection.
            if !self.skip_minor_collection {
                while changed && i < 3 {
                    let now = std::time::Instant::now();

                    log::debug!(target: "gc", "Small collection");
                    let prior_len = self.memory.len() + self.vector_cells_allocated();
                    log::debug!(target: "gc", "Previous length: {:?}", prior_len);

                    self.weak_collection();

                    let after = self.memory.len() + self.vector_cells_allocated();
                    log::debug!(target: "gc", "Objects freed: {:?}", prior_len - after);
                    log::debug!(target: "gc", "Small collection time: {:?}", now.elapsed());

                    changed = prior_len != after;

                    i += 1;
                }
            }

            let post_small_collection_size = self.memory.len() + self.vector_cells_allocated();

            let mut amount = 0;

            // Mark + Sweep!
            if post_small_collection_size as f64 > (0.25 * original_length as f64) || force_full {
                log::debug!(target: "gc", "---- Post
                    small collection,
                    running mark and sweep -
                    heap size filled: {:?} - {} ----",
                    post_small_collection_size as f64 / original_length as f64,
                    self.count);

                amount = self.mark_and_sweep(
                    root_value,
                    root_vector,
                    roots,
                    live_functions,
                    globals,
                    tls,
                    synchronizer,
                );

                if i == 1 && !changed {
                    self.skip_minor_collection = true;
                } else {
                    self.skip_minor_collection = false;
                }
            } else {
                log::debug!(target: "gc", "---- Skipping mark and sweep -
                    heap size filled: {:?} ----",
                    post_small_collection_size as f64 / original_length as f64);
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

    pub fn collection<'a>(
        &mut self,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
        force_full: bool,
    ) {
        self.value_collection(
            &SteelVal::Void,
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            force_full,
        );
    }

    // Clean up the values?
    pub fn allocate<'a>(
        &mut self,
        value: SteelVal,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> HeapRef<SteelVal> {
        self.value_collection(
            &value,
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            false,
        );
        self.memory_free_list.allocate(value)
    }

    fn value_collection<'a>(
        &mut self,
        value: &SteelVal,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &mut Synchronizer,
        force: bool,
    ) {
        if self.memory_free_list.percent_full() > 0.95 || force {
            let now = std::time::Instant::now();
            // Attempt a weak collection
            log::debug!(target: "gc", "SteelVal gc invocation");
            self.memory_free_list.weak_collection();

            log::debug!(target: "gc", "Memory size post weak collection: {}", self.memory_free_list.percent_full());
            log::debug!(target: "gc", "Weak collection time: {:?}", now.elapsed());

            if self.memory_free_list.percent_full() > 0.95 || force {
                // New generation
                self.memory_free_list.mark_all_unreachable();
                self.vector_free_list.mark_all_unreachable();

                // Just reset the counter
                let stats = self.mark_and_sweep_new(
                    Some(value.clone()),
                    std::iter::empty(),
                    roots,
                    live_functions,
                    globals,
                    tls,
                    synchronizer,
                );

                self.memory_free_list.alloc_count =
                    self.memory_free_list.elements.len() - stats.memory_reached_count;

                self.vector_free_list.alloc_count =
                    self.vector_free_list.elements.len() - stats.vector_reached_count;

                // Estimate what that memory size is?
                if self.memory_free_list.grow_count > 7 {
                    // Compact the free list.
                    self.memory_free_list.compact();
                } else {
                    self.memory_free_list.grow();
                }

                // synchronizer.resume_threads();

                log::debug!(target: "gc", "Memory size post mark and sweep: {}", self.memory_free_list.percent_full());

                log::debug!(target: "gc", "---- TOTAL GC TIME: {:?} ----", now.elapsed());
            }
        }
    }

    pub fn allocate_vector<'a>(
        &mut self,
        values: Vec<SteelVal>,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> HeapRef<Vec<SteelVal>> {
        // todo!();

        self.vector_collection(
            &values,
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            false,
        );

        // TOOD: Optimize this a lot!
        self.vector_free_list.allocate(values)
    }

    fn vector_collection<'a>(
        &mut self,
        values: &[SteelVal],
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
        force: bool,
    ) {
        if self.vector_free_list.percent_full() > 0.95 {
            let now = std::time::Instant::now();
            // Attempt a weak collection
            log::debug!(target: "gc", "Vec<SteelVal> gc invocation");
            self.vector_free_list.weak_collection();
            log::debug!(target: "gc", "Weak collection time: {:?}", now.elapsed());

            if self.vector_free_list.percent_full() > 0.95 {
                self.vector_free_list.mark_all_unreachable();

                let stats = self.mark_and_sweep_new(
                    None,
                    values.into_iter().cloned(),
                    roots,
                    live_functions,
                    globals,
                    tls,
                    synchronizer,
                );

                self.vector_free_list.alloc_count =
                    self.vector_free_list.elements.len() - stats.vector_reached_count;

                self.memory_free_list.alloc_count =
                    self.memory_free_list.elements.len() - stats.memory_reached_count;

                // if self.vector_free_list.percent_full() > 0.75 {
                if self.vector_free_list.grow_count > 7 {
                    // Compact the free list.
                    self.vector_free_list.compact();
                } else {
                    self.vector_free_list.grow();
                }
                // }

                self.vector_free_list.grow();
            }
        }
    }

    pub fn allocate_vector_iter<'a>(
        &mut self,
        values: impl Iterator<Item = SteelVal> + Clone,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> HeapRef<Vec<SteelVal>> {
        if self.vector_free_list.percent_full() > 0.95 {
            let now = std::time::Instant::now();
            // Attempt a weak collection
            log::debug!(target: "gc", "Vec<SteelVal> gc invocation");
            self.vector_free_list.weak_collection();
            log::debug!(target: "gc", "Weak collection time: {:?}", now.elapsed());

            if self.vector_free_list.percent_full() > 0.95 {
                self.vector_free_list.mark_all_unreachable();

                let stats = self.mark_and_sweep_new(
                    None,
                    values.clone(),
                    roots,
                    live_functions,
                    globals,
                    tls,
                    synchronizer,
                );

                self.vector_free_list.alloc_count =
                    self.vector_free_list.elements.len() - stats.vector_reached_count;

                self.memory_free_list.alloc_count =
                    self.memory_free_list.elements.len() - stats.memory_reached_count;

                // if self.vector_free_list.percent_full() > 0.75 {
                if self.vector_free_list.grow_count > 7 {
                    // Compact the free list.
                    self.vector_free_list.compact();
                } else {
                    self.vector_free_list.grow();
                }
                // }

                self.vector_free_list.grow();
            }
        }

        // todo!()

        // TOOD: Optimize this a lot!
        self.vector_free_list.allocate_vec(values)
    }

    fn mark_and_sweep_new<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: impl Iterator<Item = SteelVal>,
        roots: &'a [SteelVal],
        function_stack: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &mut Synchronizer,
    ) -> MarkAndSweepStats {
        let stats = self.mark(
            root_value,
            root_vector,
            roots,
            function_stack,
            globals,
            tls,
            synchronizer,
        );

        // #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        #[cfg(feature = "sync")]
        {
            GLOBAL_ROOTS.lock().unwrap().increment_generation();
        }

        #[cfg(not(feature = "sync"))]
        {
            ROOTS.with(|x| x.borrow_mut().increment_generation());
        }

        // #[cfg(feature = "profiling")]
        log::debug!(target: "gc", "Sweep: Time taken: {:?}", now.elapsed());

        synchronizer.resume_threads();

        stats

        // object_count.saturating_sub(amount_freed)
        // 0
    }

    fn mark_and_sweep<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: impl Iterator<Item = SteelVal>,
        roots: &'a [SteelVal],
        function_stack: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> usize {
        let stats = self.mark(
            root_value,
            root_vector,
            roots,
            function_stack,
            globals,
            tls,
            synchronizer,
        );

        // Turns this into a reference one:
        // let mut ref_queue = context.queue.iter().map(|x| x as _).collect();

        // let mut context = MarkAndSweepContextRef {
        //     queue: &mut ref_queue,
        //     object_count: context.object_count,
        // };

        // context.visit();

        // #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        log::debug!(target: "gc", "--- Sweeping ---");
        let prior_len = self.memory.len() + self.vector_cells_allocated();

        // sweep
        self.memory.retain(|x| x.read().is_reachable());
        self.vectors.retain(|x| x.read().is_reachable());

        let after_len = self.memory.len();

        let amount_freed = prior_len - after_len;

        log::debug!(target: "gc", "Freed objects: {:?}", amount_freed);
        log::debug!(target: "gc", "Objects alive: {:?}", after_len);

        // put them back as unreachable
        self.memory.iter().for_each(|x| x.write().reset());
        self.vectors.iter().for_each(|x| x.write().reset());

        #[cfg(feature = "sync")]
        {
            GLOBAL_ROOTS.lock().unwrap().increment_generation();
        }

        #[cfg(not(feature = "sync"))]
        {
            ROOTS.with(|x| x.borrow_mut().increment_generation());
        }

        // #[cfg(feature = "profiling")]
        log::debug!(target: "gc", "Sweep: Time taken: {:?}", now.elapsed());

        synchronizer.resume_threads();

        stats.object_count

        // 0
    }

    fn mark<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: impl Iterator<Item = SteelVal>,
        roots: &[SteelVal],
        function_stack: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &[SteelVal],
        tls: &[SteelVal],
        synchronizer: &mut Synchronizer,
    ) -> MarkAndSweepStats {
        log::debug!(target: "gc", "Marking the heap");

        // #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        /*

        let mut context = MarkAndSweepContext {
            queue: &mut self.mark_and_sweep_queue,
            object_count: 0,
        };

        // Pause all threads
        synchronizer.stop_threads();
        unsafe {
            synchronizer.enumerate_stacks(&mut context);
        }

        if let Some(root_value) = root_value {
            context.push_back(root_value);
        }

        if let Some(root_vector) = root_vector {
            for value in root_vector {
                context.push_back(value.clone());
            }
        }

        for root in tls {
            context.push_back(root.clone());
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

        #[cfg(feature = "sync")]
        {
            GLOBAL_ROOTS
                .lock()
                .unwrap()
                .roots
                .values()
                .for_each(|value| context.push_back(value.clone()))
        }

        #[cfg(not(feature = "sync"))]
        {
            ROOTS.with(|x| {
                x.borrow()
                    .roots
                    .values()
                    .for_each(|value| context.push_back(value.clone()))
            });
        }

        context.visit();
        */

        let mut context = MarkAndSweepContext {
            queue: &mut self.mark_and_sweep_queue,
            object_count: 0,
        };

        // Pause all threads
        synchronizer.stop_threads();
        unsafe {
            synchronizer.enumerate_stacks(&mut context);
        }

        if let Some(root_value) = root_value {
            context.push_back(root_value);
        }

        for value in root_vector {
            context.push_back(value.clone());
        }

        for root in tls {
            context.push_back(root.clone());
        }

        log::debug!(target: "gc", "Roots size: {}", roots.len());
        for root in roots {
            context.push_back(root.clone());
        }

        log::debug!(target: "gc", "Globals size: {}", globals.len());
        for root in globals {
            context.push_back(root.clone());
        }

        for function in function_stack {
            // for heap_ref in function.heap_allocated.borrow().iter() {
            //     context.mark_heap_reference(&heap_ref.strong_ptr())
            // }

            for value in function.captures() {
                context.push_back(value.clone());
            }
        }

        #[cfg(feature = "sync")]
        {
            GLOBAL_ROOTS
                .lock()
                .unwrap()
                .roots
                .values()
                .for_each(|value| context.push_back(value.clone()))
        }

        #[cfg(not(feature = "sync"))]
        {
            ROOTS.with(|x| {
                x.borrow()
                    .roots
                    .values()
                    .for_each(|value| context.push_back(value.clone()))
            });
        }

        // TODO: Can we do this in parallel? Divide up the iterator into separate components
        // and do them that way?
        log::debug!(target: "gc", "Stack size: {}", context.queue.len());

        // Divide up the queue - manage the thread allocation ourselves for
        // better use.
        const USE_SHARED_QUEUE: bool = false;

        let count = MARKER.mark(&context.queue);
        log::debug!(target: "gc", "Mark: Time taken: {:?}", now.elapsed());
        count
    }
}

struct VecWrapper(Vec<*const SteelVal>);

unsafe impl Sync for VecWrapper {}
unsafe impl Send for VecWrapper {}

// Shared resource?
#[derive(Clone)]
struct ParallelMarker {
    // Chunks of the roots for marking
    senders: Arc<Mutex<Vec<MarkerWorker>>>,
    queue: Arc<crossbeam_queue::SegQueue<SteelValPointer>>,
}

unsafe impl Sync for ParallelMarker {}
unsafe impl Send for ParallelMarker {}

struct MarkerWorker {
    // Tell the thread to wake up
    sender: Sender<()>,
    ack: Receiver<MarkAndSweepStats>,
    handle: JoinHandle<()>,
}

impl ParallelMarker {
    pub fn new() -> Self {
        // No parallelism
        let parallelism = std::thread::available_parallelism()
            .map(|x| x.get() + 1)
            .unwrap_or(1);

        let mut workers = Vec::with_capacity(parallelism);
        let queue = Arc::new(crossbeam_queue::SegQueue::new());

        for _ in 0..parallelism {
            let cloned_queue = queue.clone();
            let (sender, receiver) = crossbeam_channel::unbounded();
            let (ack_sender, ack_receiver) = crossbeam_channel::unbounded();

            let handle = std::thread::spawn(move || {
                let cloned_queue = cloned_queue;
                let mut local_queue = Vec::with_capacity(4096);
                for _ in receiver {
                    let now = std::time::Instant::now();

                    let mut context = MarkAndSweepContextRefQueue {
                        queue: &cloned_queue,
                        keep_alive: Vec::new(),
                        // Allocate this queue once, don't keep allocating per thread.
                        local_queue: &mut local_queue,
                        stats: MarkAndSweepStats::default(),
                    };

                    context.visit();

                    log::debug!(target: "gc",
                        "{:?}: {:?} -> {:?}",
                        std::thread::current().id(),
                        now.elapsed(),
                        context.stats
                    );

                    ack_sender.send(context.stats).unwrap();
                }
            });

            workers.push(MarkerWorker {
                sender,
                handle,
                ack: ack_receiver,
            });
        }

        Self {
            senders: Arc::new(Mutex::new(workers)),
            queue,
        }
    }

    pub fn mark(&self, queue: &Vec<SteelVal>) -> MarkAndSweepStats {
        let guard = self.senders.lock().unwrap();

        for value in queue.iter() {
            if let Some(p) = SteelValPointer::from_value(value) {
                // Start with a local queue first?
                self.queue.push(p);
            }
        }

        for worker in guard.iter() {
            worker.sender.send(()).unwrap();
        }

        let mut count = MarkAndSweepStats::default();

        for worker in guard.iter() {
            count = count + worker.ack.recv().unwrap();
        }

        count
    }
}

pub trait HeapAble: Clone + std::fmt::Debug + PartialEq + Eq {
    fn empty() -> Self;
}
impl HeapAble for SteelVal {
    fn empty() -> Self {
        SteelVal::Void
    }
}
impl HeapAble for Vec<SteelVal> {
    fn empty() -> Self {
        Self::new()
    }
}

// fn test_borrow() {
//     let mut value = vec![1, 2, 3, 4, 5];
//     HeapAble::reuse(&mut value, &[1, 2, 3, 4, 5]);
// }

#[derive(Clone, Debug)]
pub struct HeapRef<T: HeapAble> {
    pub(crate) inner: WeakShared<MutContainer<HeapAllocated<T>>>,
}

const USE_UNSAFE: bool = true;

impl<T: HeapAble> HeapRef<T> {
    #[inline(always)]
    pub fn get(&self) -> T {
        // Atomic cell instead? That way we can just do an atomic swap rather than a rw lock?
        if USE_UNSAFE {
            unsafe { &*self.inner.as_ptr() }.read().value.clone()
        } else {
            self.inner.upgrade().unwrap().read().value.clone()
        }
    }

    /// Get the value if the pointer is still valid.
    /// If this is the only thing pointing at it, then we need to drop it.
    pub(crate) fn maybe_get_from_weak(&self) -> Option<T> {
        let inner = self.inner.upgrade()?;

        // Check if this thing is reachable? How?
        if StandardShared::weak_count(&inner) == 1 {
            // We can go ahead and nuke this since we're
            // the only things pointing to it
            let mut value = inner.write();

            if value.is_reachable() {
                return Some(value.value.clone());
            } else {
                value.reachable = false;
                value.value = T::empty();
                return None;
            }
        } else {
            // Get the inner value on this, assuming we are calling
            // this just from the heap ref. Anywhere else, we'll want
            // to eliminate this?
            Some(inner.read().value.clone())
        }
    }

    pub fn borrow<O>(&self, thunk: impl FnOnce(&T) -> O) -> O {
        let value = self.inner.upgrade().unwrap();
        let value = value.read();
        thunk(&value.value)
    }

    pub fn as_ptr_usize(&self) -> usize {
        self.inner.as_ptr() as usize
    }

    pub fn set(&mut self, value: T) -> T {
        let inner = self.inner.upgrade().unwrap();

        let ret = { inner.read().value.clone() };

        inner.write().value = value;
        ret
    }

    pub fn set_and_return(&self, value: T) -> T {
        let inner = self.inner.upgrade().unwrap();

        let mut guard = inner.write();
        std::mem::replace(&mut guard.value, value)
    }

    pub(crate) fn set_interior_mut(&self, value: T) -> T {
        let inner = self.inner.upgrade().unwrap();

        let ret = { inner.read().value.clone() };

        inner.write().value = value;
        ret
    }

    pub(crate) fn strong_ptr(&self) -> StandardSharedMut<HeapAllocated<T>> {
        self.inner.upgrade().unwrap()
    }

    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        WeakShared::ptr_eq(&self.inner, &other.inner)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HeapAllocated<T: Clone + std::fmt::Debug + PartialEq + Eq> {
    // TODO: We have room for more bits here. We can add a destructor
    // flag and have minor collections run more frequently on allocation.
    //
    // These will send the unreachable values out of view to a background
    // thread where the value will _then_ be called with a destructor.
    //
    // Also add a flag for whether this is pointed to be a weak box.
    // This can let us eagerly free on a weak collection, since anything
    // thrown inside the box could stop being help reasonably quickly and
    // would allow us to avoid running a gc collection to free things.
    pub(crate) reachable: bool,

    pub(crate) finalizer: bool,

    pub(crate) value: T,
}

#[test]
fn check_size_of_heap_allocated_value() {
    println!("{:?}", std::mem::size_of::<HeapAllocated<SteelVal>>());
    // println!("{:?}", std::mem::size_of::<Test>());
}

impl<T: Clone + std::fmt::Debug + PartialEq + Eq> HeapAllocated<T> {
    pub fn new(value: T) -> Self {
        Self {
            reachable: false,
            finalizer: false,
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
    pub(crate) fn mark_heap_reference(
        &mut self,
        heap_ref: &StandardSharedMut<HeapAllocated<SteelVal>>,
    ) {
        if heap_ref.read().is_reachable() {
            return;
        }

        {
            heap_ref.write().mark_reachable();
        }

        self.push_back(heap_ref.read().value.clone());
    }

    // Visit the heap vector, mark it as visited!
    pub(crate) fn mark_heap_vector(
        &mut self,
        heap_vector: &StandardSharedMut<HeapAllocated<Vec<SteelVal>>>,
    ) {
        if heap_vector.read().is_reachable() {
            return;
        }

        {
            heap_vector.write().mark_reachable();
        }

        for value in heap_vector.read().value.iter() {
            self.push_back(value.clone());
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct MarkAndSweepStats {
    object_count: usize,
    memory_reached_count: usize,
    vector_reached_count: usize,
}

impl std::ops::Add for MarkAndSweepStats {
    type Output = MarkAndSweepStats;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.object_count += rhs.object_count;
        self.memory_reached_count += rhs.memory_reached_count;
        self.vector_reached_count += rhs.vector_reached_count;
        self
    }
}

// TODO: This should still use the queue, but instead we have to stash object with
// which we have to acquire a read guard on. Otherwise it is possible we'll be
// reading from invalid pointers.
pub struct MarkAndSweepContextRefQueue<'a> {
    // Thread local queue + larger queue when it runs out?
    // Try to push on to local queue first.
    local_queue: &'a mut Vec<SteelValPointer>,
    keep_alive: Vec<SteelVal>,
    queue: &'a crossbeam_queue::SegQueue<SteelValPointer>,
    stats: MarkAndSweepStats,
}

impl<'a> MarkAndSweepContextRefQueue<'a> {
    pub(crate) fn mark_heap_reference(
        &mut self,
        heap_ref: &StandardSharedMut<HeapAllocated<SteelVal>>,
    ) {
        if heap_ref.read().is_reachable() {
            return;
        }

        {
            heap_ref.write().mark_reachable();
        }

        self.stats.memory_reached_count += 1;

        self.push_back(&heap_ref.read().value);
    }

    // Visit the heap vector, mark it as visited!
    pub(crate) fn mark_heap_vector(
        &mut self,
        heap_vector: &StandardSharedMut<HeapAllocated<Vec<SteelVal>>>,
    ) {
        if heap_vector.read().is_reachable() {
            return;
        }

        {
            heap_vector.write().mark_reachable();
        }

        self.stats.vector_reached_count += 1;

        for value in heap_vector.read().value.iter() {
            self.push_back(&value);
        }
    }

    pub(crate) fn save(&mut self, value: SteelVal) {
        self.keep_alive.push(value);
    }
}

impl<'a> BreadthFirstSearchSteelValVisitor for MarkAndSweepContext<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    // TODO: Do this in parallel, if possible?
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
    fn visit_boxed_function(&mut self, _function: Gc<BoxedDynFunction>) -> Self::Output {}
    // TODO: Revisit this when the boxed iterator is cleaned up
    fn visit_boxed_iterator(&mut self, iterator: GcMut<OpaqueIterator>) -> Self::Output {
        self.push_back(iterator.read().root.clone());
    }
    fn visit_boxed_value(&mut self, boxed_value: GcMut<SteelVal>) -> Self::Output {
        self.push_back(boxed_value.read().clone());
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
        let continuation = (*continuation.inner.read()).clone();

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

                    // if let Some(handler) = &frame.handler {
                    //     self.push_back((*handler.as_ref()).clone());
                    // }

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
    fn visit_custom_type(&mut self, custom_type: GcMut<Box<dyn CustomType>>) -> Self::Output {
        custom_type.read().visit_children(self);
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
    fn visit_reference_value(&mut self, _reference: Gc<OpaqueReference<'static>>) -> Self::Output {}

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

impl<'a> BreadthFirstSearchSteelValReferenceVisitor2<'a> for MarkAndSweepContextRefQueue<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {
        ()
    }

    fn pop_front(&mut self) -> Option<SteelValPointer> {
        self.local_queue.pop().or_else(|| self.queue.pop())
    }

    fn push_back(&mut self, value: &SteelVal) {
        self.stats.object_count += 1;

        match value {
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
                if let Some(p) = SteelValPointer::from_value(value) {
                    if self.local_queue.len() == self.local_queue.capacity() {
                        self.queue.push(p);
                    } else {
                        self.local_queue.push(p);
                    }
                }
            }
        }
    }

    // TODO: Revisit this when the boxed iterator is cleaned up
    fn visit_boxed_iterator(&mut self, iterator: &MutContainer<OpaqueIterator>) -> Self::Output {
        let guard = iterator.read();
        self.save(guard.root.clone());
        self.push_back(&guard.root);
    }

    fn visit_boxed_value(&mut self, boxed_value: &MutContainer<SteelVal>) -> Self::Output {
        let guard = boxed_value.read();
        self.save(guard.clone());
        self.push_back(&guard);
    }

    fn visit_closure(&mut self, closure: &ByteCodeLambda) -> Self::Output {
        for capture in closure.captures() {
            self.push_back(&capture);
        }

        if let Some(contract) = closure.get_contract_information().as_ref() {
            self.push_back(&contract);
        }
    }
    fn visit_continuation(
        &mut self,
        continuation: &MutContainer<ContinuationMark>,
    ) -> Self::Output {
        // TODO: Don't clone this here!
        let continuation = continuation.read();

        match &(*continuation) {
            ContinuationMark::Closed(continuation) => {
                for value in &continuation.stack {
                    self.save(value.clone());
                    self.push_back(&value);
                }

                for value in &continuation.current_frame.function.captures {
                    self.save(value.clone());
                    self.push_back(&value);
                }

                for frame in &continuation.stack_frames {
                    for value in &frame.function.captures {
                        self.save(value.clone());
                        self.push_back(&value);
                    }

                    if let Some(handler) =
                        frame.attachments.as_ref().and_then(|x| x.handler.clone())
                    {
                        self.save(handler.clone());
                        self.push_back(&handler);
                    }
                }
            }

            ContinuationMark::Open(continuation) => {
                for value in &continuation.current_stack_values {
                    self.save(value.clone());
                    self.push_back(value);
                }

                for value in &continuation.current_frame.function.captures {
                    self.save(value.clone());
                    self.push_back(value);
                }
            }
        }
    }
    // TODO: Come back to this?
    fn visit_custom_type(
        &mut self,
        custom_type: &'a MutContainer<Box<dyn CustomType>>,
    ) -> Self::Output {
        // Use mark and sweep queue:
        let mut queue = Vec::new();
        let mut temporary_queue = MarkAndSweepContext {
            queue: &mut queue,
            object_count: 0,
        };

        // Intercept the downstream ones, keep them alive by using
        // a local queue first, and then spilling over to the larger one.
        custom_type.read().visit_children(&mut temporary_queue);

        for value in queue {
            self.push_back(&value);
            self.save(value);
        }
    }

    fn visit_hash_map(&mut self, hashmap: &super::HashMap<SteelVal, SteelVal>) -> Self::Output {
        for (key, value) in hashmap.iter() {
            self.push_back(key);
            self.push_back(value);
        }
    }

    fn visit_hash_set(&mut self, hashset: &super::HashSet<SteelVal>) -> Self::Output {
        for value in hashset.iter() {
            self.push_back(value);
        }
    }

    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        self.mark_heap_reference(&heap_ref.strong_ptr());
    }

    fn visit_immutable_vector(&mut self, vector: &Vector<SteelVal>) -> Self::Output {
        for value in vector.iter() {
            self.push_back(value);
        }
    }
    fn visit_list(&mut self, list: super::lists::CellPointer<SteelVal>) -> Self::Output {
        unsafe {
            super::lists::List::<SteelVal>::call_from_raw(list, |lst| {
                for value in lst {
                    self.push_back(value);
                }
            })
        }
    }

    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        self.mark_heap_vector(&vector.strong_ptr())
    }

    fn visit_reducer(&mut self, reducer: &Reducer) -> Self::Output {
        match reducer {
            Reducer::ForEach(f) => self.push_back(&f),
            Reducer::Generic(rf) => {
                self.push_back(&rf.initial_value);
                self.push_back(&rf.function);
            }
            _ => {}
        }
    }

    fn visit_steel_struct(&mut self, steel_struct: &UserDefinedStruct) -> Self::Output {
        for field in steel_struct.fields.iter() {
            self.push_back(&field);
        }
    }

    fn visit_stream(&mut self, stream: &LazyStream) -> Self::Output {
        self.push_back(&stream.initial_value);
        self.push_back(&stream.stream_thunk);
    }

    fn visit_syntax_object(&mut self, syntax_object: &Syntax) -> Self::Output {
        if let Some(raw) = &syntax_object.raw {
            self.push_back(raw);
        }

        self.push_back(&syntax_object.syntax);
    }

    fn visit_transducer(&mut self, transducer: &Transducer) -> Self::Output {
        for transducer in transducer.ops.iter() {
            match &transducer {
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

    fn visit_pair(&mut self, pair: &super::lists::Pair) -> Self::Output {
        self.push_back(pair.car_ref());
        self.push_back(pair.cdr_ref());
    }
}
