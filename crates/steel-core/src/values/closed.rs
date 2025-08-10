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
        cycles::{
            BreadthFirstSearchSteelValReferenceVisitor,
            BreadthFirstSearchSteelValReferenceVisitor2, BreadthFirstSearchSteelValVisitor2,
        },
        OpaqueIterator, SteelComplex, SteelValPointer, SteelVector,
    },
    steel_vm::vm::{Continuation, ContinuationMark, Synchronizer},
    values::lists::List,
};
use crossbeam_channel::{Receiver, Sender};
use crossbeam_queue::SegQueue;
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

// Have free list for vectors and values separately. Can keep some of the vectors pre allocated
// as well, as necessary
#[derive(Debug)]
struct FreeList<T: HeapAble> {
    // TODO: When we've moved into the from space, we can allocate
    // explicitly from there, before moving on back to the to space.
    //
    // It should be reasonable to allocate (reuse) a field that simply has been
    // marked as not able to be reached.
    //
    // We need 3 modes, Reachable, Maybe reachable, unreachable.
    // Anything marks as reachable
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
        // Can probably make this a lot bigger
        let current = self.elements.len().max(Self::EXTEND_CHUNK);

        self.cursor = self.elements.len();

        self.elements.reserve(current);
        self.elements.extend(
            std::iter::repeat_with(|| {
                StandardShared::new(MutContainer::new(HeapAllocated::new(T::empty())))
            })
            .take(current),
        );

        log::debug!(target: "gc", "Growing the heap by: {}", current);

        self.alloc_count += current;
        self.grow_count += 1;

        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].read().is_reachable());
        }
    }

    // Extend the heap
    fn extend_heap(&mut self) {
        // self.cursor = self.elements.len();
        self.grow();

        // TODO: Check that this makes sense?
        // self.cursor = self
        //     .elements
        //     .iter()
        //     .position(|x| !x.read().is_reachable())
        //     .unwrap();

        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].read().is_reachable());
        }
    }

    // TODO: Allocate and also mark the roots when we're full!
    fn allocate(&mut self, value: T) -> HeapRef<T> {
        // if self.cursor == 0 {
        //     dbg!(&self.elements);
        // }

        // TODO: Figure out why the cursor is off?
        // Seek to the next free thing.
        // self.cursor += self.elements[self.cursor..]
        //     .iter()
        //     .position(|x| !x.read().is_reachable())
        //     .unwrap();

        // dbg!(self.cursor);
        // dbg!(&self.elements[self.cursor]);

        // Drain, moving values around...
        // is that expensive?
        let guard = &mut self.elements[self.cursor];

        // Allocate into this field
        let mut heap_guard = guard.write();

        heap_guard.value = value;

        // assert!(!heap_guard.reachable);

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
    pub fn allocate_old<'a>(
        &mut self,
        value: SteelVal,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> HeapRef<SteelVal> {
        self.collect(
            Some(value.clone()),
            None,
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            false,
        );

        let pointer = StandardShared::new(MutContainer::new(HeapAllocated::new(value)));
        let weak_ptr = StandardShared::downgrade(&pointer);

        self.memory.push(pointer);

        HeapRef { inner: weak_ptr }
    }

    pub fn allocate_without_collection<'a>(&mut self, value: SteelVal) -> HeapRef<SteelVal> {
        let pointer = StandardShared::new(MutContainer::new(HeapAllocated::new(value)));
        let weak_ptr = StandardShared::downgrade(&pointer);

        self.memory.push(pointer);

        HeapRef { inner: weak_ptr }
    }

    // Allocate a vector explicitly onto the heap
    pub fn allocate_vector_old<'a>(
        &mut self,
        values: Vec<SteelVal>,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> HeapRef<Vec<SteelVal>> {
        self.collect(
            None,
            Some(&values),
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            false,
        );

        let pointer = StandardShared::new(MutContainer::new(HeapAllocated::new(values)));
        let weak_ptr = StandardShared::downgrade(&pointer);

        self.vectors.push(pointer);

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
        root_vector: Option<&Vec<SteelVal>>,
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
        if self.memory_free_list.percent_full() > 0.95 {
            // Attempt a weak collection
            log::debug!(target: "gc", "SteelVal gc invocation");
            self.memory_free_list.weak_collection();

            log::debug!(target: "gc", "Memory size post weak collection: {}", self.memory_free_list.percent_full());

            if self.memory_free_list.percent_full() > 0.95 {
                // New generation
                self.memory_free_list.mark_all_unreachable();

                // Just reset the counter
                self.mark_and_sweep_new(
                    Some(value.clone()),
                    None,
                    roots,
                    live_functions,
                    globals,
                    tls,
                    synchronizer,
                );

                self.memory_free_list.recount();

                // Estimate what that memory size is?
                if self.memory_free_list.grow_count > 7 {
                    // Compact the free list.
                    self.memory_free_list.compact();
                } else {
                    self.memory_free_list.grow();
                }

                // synchronizer.resume_threads();

                log::debug!(target: "gc", "Memory size post mark and sweep: {}", self.memory_free_list.percent_full());
            }
        }

        self.memory_free_list.allocate(value)
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
        if self.vector_free_list.percent_full() > 0.95 {
            // Attempt a weak collection
            log::debug!(target: "gc", "Vec<SteelVal> gc invocation");
            self.vector_free_list.weak_collection();

            if self.vector_free_list.percent_full() > 0.95 {
                self.vector_free_list.mark_all_unreachable();

                self.mark_and_sweep_new(
                    None,
                    Some(&values),
                    roots,
                    live_functions,
                    globals,
                    tls,
                    synchronizer,
                );

                self.vector_free_list.recount();

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

        // TOOD: Optimize this a lot!
        self.vector_free_list.allocate(values)
    }

    fn mark_and_sweep_new<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: Option<&Vec<SteelVal>>,
        roots: &'a [SteelVal],
        function_stack: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &mut Synchronizer,
    ) {
        let (mut context, count) = self.mark(
            root_value,
            root_vector,
            roots,
            function_stack,
            globals,
            tls,
            synchronizer,
        );

        context.object_count = count;

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

        // object_count.saturating_sub(amount_freed)
        // 0
    }

    fn mark_and_sweep<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: Option<&Vec<SteelVal>>,
        roots: &'a [SteelVal],
        function_stack: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> usize {
        let (mut context, count) = self.mark(
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

        context.object_count = count;

        // #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        let object_count = context.object_count;

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

        object_count.saturating_sub(amount_freed)
        // 0
    }

    fn mark<'a>(
        &mut self,
        root_value: Option<SteelVal>,
        root_vector: Option<&Vec<SteelVal>>,
        roots: &[SteelVal],
        function_stack: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &[SteelVal],
        tls: &[SteelVal],
        synchronizer: &mut Synchronizer,
    ) -> (MarkAndSweepContext<'_>, usize) {
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

        if let Some(root_vector) = root_vector {
            for value in root_vector {
                context.push_back(value.clone());
            }
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
        const USE_SCOPED_THREADS: bool = false;
        const USE_SHARED_QUEUE: bool = true;

        let count = if USE_SCOPED_THREADS {
            std::thread::scope(|s| {
                let mut handles = Vec::new();

                let chunk_size = std::thread::available_parallelism()
                    .map(|x| context.queue.len() / x)
                    .unwrap_or(256);

                let now = std::time::Instant::now();

                // Divide into n cores?
                for chunk in context.queue.chunks(chunk_size) {
                    let handle = s.spawn(|| {
                        let mut ref_queue = chunk.iter().map(|x| x as _).collect();

                        let mut context = MarkAndSweepContextRef {
                            queue: &mut ref_queue,
                            object_count: 0,
                        };

                        context.visit();

                        context.object_count
                    });

                    handles.push(handle);
                }

                log::debug!(target: "gc", "Mark: Thread spawning time: {:?}", now.elapsed());

                let mut count = 0;
                for handle in handles {
                    count += handle.join().unwrap();
                }

                count
            })
        } else if USE_SHARED_QUEUE {
            let queue = SegQueue::new();
            for value in context.queue.iter() {
                if let Some(p) = SteelValPointer::from_value(value) {
                    queue.push(p);
                }
            }

            // let queue = Mutex::new(queue);

            std::thread::scope(|s| {
                let mut handles = Vec::new();

                let chunk_size = std::thread::available_parallelism()
                    .map(|x| x.get())
                    .unwrap_or(1);

                let now = std::time::Instant::now();

                for _ in 0..chunk_size {
                    let handle = s.spawn(|| {
                        let now = std::time::Instant::now();
                        let mut context = MarkAndSweepContextRefQueue {
                            local_queue: Vec::with_capacity(4096),
                            queue: &queue,
                            object_count: 0,
                        };

                        context.visit();
                        log::debug!(target: "gc", "Thread: {:?} -> {:?}", std::thread::current().id(), now.elapsed());

                        context.object_count
                    });

                    handles.push(handle);
                }

                log::debug!(target: "gc", "Mark: Thread spawning time: {:?}", now.elapsed());

                let mut count = 0;
                for handle in handles {
                    count += handle.join().unwrap();
                }

                count
            })
        } else {
            MARKER.mark(&context.queue)
        };

        log::debug!(target: "gc", "Mark: Time taken: {:?}", now.elapsed());
        (context, count)
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
    queue: Arc<crossbeam_queue::SegQueue<PointerWrapper>>,
}

unsafe impl Sync for ParallelMarker {}
unsafe impl Send for ParallelMarker {}

struct MarkerWorker {
    sender: Sender<VecWrapper>,
    ack: Receiver<usize>,
    handle: JoinHandle<()>,
}

struct PointerWrapper(*const SteelVal);
unsafe impl Sync for PointerWrapper {}
unsafe impl Send for PointerWrapper {}

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
            let (sender, receiver) = crossbeam_channel::unbounded::<VecWrapper>();
            let (ack_sender, ack_receiver) = crossbeam_channel::unbounded();
            let handle = std::thread::spawn(move || {
                let cloned_queue = cloned_queue;
                for chunk in receiver {
                    let now = std::time::Instant::now();

                    for c in chunk.0 {
                        cloned_queue.push(PointerWrapper(c));
                    }

                    let mut context = MarkAndSweepContextRefQueue {
                        queue: todo!(),
                        object_count: 0,
                        // TODO: Check the capacity here
                        local_queue: Vec::new(),
                    };

                    context.visit();

                    println!(
                        "{:?}: {:?} -> {}",
                        std::thread::current().id(),
                        now.elapsed(),
                        context.object_count
                    );

                    ack_sender.send(context.object_count).unwrap();
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

    pub fn mark(&self, queue: &Vec<SteelVal>) -> usize {
        let guard = self.senders.lock().unwrap();

        let denominator = (guard.len() - 1).max(1);

        let chunk_size = queue.len() / denominator;

        let chunk_iter = queue.chunks(chunk_size);

        dbg!(queue.chunks(chunk_size).count());
        dbg!(guard.len());
        assert!(queue.chunks(chunk_size).count() <= guard.len());

        for (chunk, worker) in chunk_iter.zip(guard.iter()) {
            let chunk = VecWrapper(chunk.iter().map(|x| x as _).collect());
            worker.sender.send(chunk).unwrap();
        }

        let mut count = 0;

        for worker in guard.iter() {
            count += worker.ack.recv().unwrap();
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

#[derive(Clone, Debug)]
pub struct HeapRef<T: HeapAble> {
    pub(crate) inner: WeakShared<MutContainer<HeapAllocated<T>>>,
}

impl<T: HeapAble> HeapRef<T> {
    #[inline(always)]
    pub fn get(&self) -> T {
        self.inner.upgrade().unwrap().read().value.clone()
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

impl<'a> MarkAndSweepContextRef<'a> {
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

        for value in heap_vector.read().value.iter() {
            self.push_back(&value);
        }
    }
}

pub struct MarkAndSweepContextRef<'a> {
    queue: &'a mut Vec<*const SteelVal>,
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

pub struct MarkAndSweepContextRefQueue<'a> {
    // Thread local queue + larger queue when it runs out?
    // Try to push on to local queue first.
    local_queue: Vec<SteelValPointer>,
    queue: &'a crossbeam_queue::SegQueue<SteelValPointer>,
    object_count: usize,
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

        for value in heap_vector.read().value.iter() {
            self.push_back(&value);
        }
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

impl<'a> BreadthFirstSearchSteelValReferenceVisitor<'a> for MarkAndSweepContextRef<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {
        ()
    }

    fn pop_front(&mut self) -> Option<*const SteelVal> {
        self.queue.pop()
    }

    fn visit(&mut self) -> Self::Output {
        use SteelVal::*;

        let mut ret = self.default_output();

        // Check the queue here, and otherwise send to a background worker for when the task is complete?
        // Check if queue is empty, forward to queue?

        while let Some(value) = self.pop_front() {
            ret = match unsafe { &(*value) } {
                Closure(c) => self.visit_closure(c),
                BoolV(b) => self.visit_bool(*b),
                NumV(n) => self.visit_float(*n),
                IntV(i) => self.visit_int(*i),
                Rational(x) => self.visit_rational(*x),
                BigRational(x) => self.visit_bigrational(x),
                Complex(_) => unimplemented!(),
                CharV(c) => self.visit_char(*c),
                VectorV(v) => self.visit_immutable_vector(v),
                Void => self.visit_void(),
                StringV(s) => self.visit_string(s),
                FuncV(f) => self.visit_function_pointer(*f),
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
                BigNum(b) => self.visit_bignum(b),
                HeapAllocated(b) => self.visit_heap_allocated(b),
                Pair(p) => self.visit_pair(p),
                ByteVector(b) => self.visit_bytevector(b),
            };
        }

        ret
    }

    fn push_back(&mut self, value: &SteelVal) {
        self.object_count += 1;

        // TODO: Determine if all numbers should push back.
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
                self.queue.push(value as _);
            }
        }

        // if self.queue.len() > 4096 {
        //     log::debug!(target: "gc", "Thread: {:?} -> {}", std::thread::current().id(), self.queue.len());
        // }

        // If the queue is big enough, chunk it up and send it on the queue?
    }

    fn visit_bytevector(&mut self, _bytevector: &crate::rvals::SteelByteVector) -> Self::Output {}
    fn visit_bignum(&mut self, _: &Gc<BigInt>) -> Self::Output {}
    fn visit_bool(&mut self, _boolean: bool) -> Self::Output {}
    fn visit_boxed_function(&mut self, _function: &Gc<BoxedDynFunction>) -> Self::Output {}
    // TODO: Revisit this when the boxed iterator is cleaned up
    fn visit_boxed_iterator(&mut self, iterator: &GcMut<OpaqueIterator>) -> Self::Output {
        self.push_back(&iterator.read().root);
    }
    fn visit_boxed_value(&mut self, boxed_value: &GcMut<SteelVal>) -> Self::Output {
        self.push_back(&boxed_value.read());
    }

    fn visit_builtin_function(&mut self, _function: &BuiltInSignature) -> Self::Output {}

    fn visit_char(&mut self, _c: char) -> Self::Output {}
    fn visit_closure(&mut self, closure: &Gc<ByteCodeLambda>) -> Self::Output {
        // for heap_ref in closure.heap_allocated.borrow().iter() {
        //     self.mark_heap_reference(&heap_ref.strong_ptr())
        // }

        for capture in closure.captures() {
            self.push_back(capture);
        }

        if let Some(contract) = closure.get_contract_information().as_ref() {
            self.push_back(contract);
        }
    }
    fn visit_continuation(&mut self, continuation: &Continuation) -> Self::Output {
        // TODO: Don't clone this here!
        let continuation = continuation.inner.read();

        match &(*continuation) {
            ContinuationMark::Closed(continuation) => {
                for value in &continuation.stack {
                    self.push_back(value);
                }

                for value in &continuation.current_frame.function.captures {
                    self.push_back(value);
                }

                for frame in &continuation.stack_frames {
                    for value in &frame.function.captures {
                        self.push_back(value);
                    }

                    // if let Some(handler) = &frame.handler {
                    //     self.push_back((*handler.as_ref()).clone());
                    // }

                    if let Some(handler) =
                        frame.attachments.as_ref().and_then(|x| x.handler.clone())
                    {
                        self.push_back(&handler);
                    }
                }
            }

            ContinuationMark::Open(continuation) => {
                for value in &continuation.current_stack_values {
                    self.push_back(value);
                }

                for value in &continuation.current_frame.function.captures {
                    self.push_back(value);
                }
            }
        }
    }
    // TODO: Come back to this
    fn visit_custom_type(&mut self, custom_type: &GcMut<Box<dyn CustomType>>) -> Self::Output {
        // todo!()
        custom_type.read().visit_children_ref(self);
    }

    fn visit_float(&mut self, _float: f64) -> Self::Output {}

    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) -> Self::Output {}

    fn visit_future(&mut self, _future: &Gc<FutureResult>) -> Self::Output {}

    fn visit_future_function(&mut self, _function: &BoxedAsyncFunctionSignature) -> Self::Output {}

    fn visit_hash_map(&mut self, hashmap: &SteelHashMap) -> Self::Output {
        for (key, value) in hashmap.iter() {
            self.push_back(key);
            self.push_back(value);
        }
    }

    fn visit_hash_set(&mut self, hashset: &SteelHashSet) -> Self::Output {
        for value in hashset.iter() {
            self.push_back(value);
        }
    }

    fn visit_heap_allocated(&mut self, heap_ref: &HeapRef<SteelVal>) -> Self::Output {
        self.mark_heap_reference(&heap_ref.strong_ptr());
    }

    fn visit_immutable_vector(&mut self, vector: &SteelVector) -> Self::Output {
        for value in vector.iter() {
            self.push_back(value);
        }
    }
    fn visit_int(&mut self, _int: isize) -> Self::Output {}
    fn visit_rational(&mut self, _: Rational32) -> Self::Output {}
    fn visit_bigrational(&mut self, _: &Gc<BigRational>) -> Self::Output {}

    fn visit_list(&mut self, list: &List<SteelVal>) -> Self::Output {
        for value in list {
            self.push_back(value);
        }
    }

    fn visit_mutable_function(&mut self, _function: &MutFunctionSignature) -> Self::Output {}

    fn visit_mutable_vector(&mut self, vector: &HeapRef<Vec<SteelVal>>) -> Self::Output {
        self.mark_heap_vector(&vector.strong_ptr())
    }

    fn visit_port(&mut self, _port: &SteelPort) -> Self::Output {}

    fn visit_reducer(&mut self, reducer: &Gc<Reducer>) -> Self::Output {
        match reducer.as_ref() {
            Reducer::ForEach(f) => self.push_back(f),
            Reducer::Generic(rf) => {
                self.push_back(&rf.initial_value);
                self.push_back(&rf.function);
            }
            _ => {}
        }
    }

    // TODO: Revisit this
    fn visit_reference_value(&mut self, _reference: &Gc<OpaqueReference<'static>>) -> Self::Output {
    }

    fn visit_steel_struct(&mut self, steel_struct: &Gc<UserDefinedStruct>) -> Self::Output {
        for field in steel_struct.fields.iter() {
            self.push_back(field);
        }
    }

    fn visit_stream(&mut self, stream: &Gc<LazyStream>) -> Self::Output {
        self.push_back(&stream.initial_value);
        self.push_back(&stream.stream_thunk);
    }

    fn visit_string(&mut self, _string: &SteelString) -> Self::Output {}

    fn visit_symbol(&mut self, _symbol: &SteelString) -> Self::Output {}

    fn visit_syntax_object(&mut self, syntax_object: &Gc<Syntax>) -> Self::Output {
        if let Some(raw) = &syntax_object.raw {
            self.push_back(raw);
        }

        self.push_back(&syntax_object.syntax);
    }

    fn visit_transducer(&mut self, transducer: &Gc<Transducer>) -> Self::Output {
        for transducer in transducer.ops.iter() {
            match transducer {
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

    fn visit_pair(&mut self, pair: &Gc<super::lists::Pair>) -> Self::Output {
        self.push_back(pair.car_ref());
        self.push_back(pair.cdr_ref());
    }
}

// BreadthFirstSearchSteelValVisitor
// impl<'a> BreadthFirstSearchSteelValReferenceVisitor<'a> for MarkAndSweepContextRefQueue<'a> {
impl<'a> BreadthFirstSearchSteelValReferenceVisitor2<'a> for MarkAndSweepContextRefQueue<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {
        ()
    }

    fn pop_front(&mut self) -> Option<SteelValPointer> {
        self.local_queue.pop().or_else(|| self.queue.pop())
    }

    fn push_back(&mut self, value: &SteelVal) {
        self.object_count += 1;

        // TODO: Determine if all numbers should push back.
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

        // if self.queue.len() > 4096 {
        //     log::debug!(target: "gc", "Thread: {:?} -> {}", std::thread::current().id(), self.queue.len());
        // }

        // If the queue is big enough, chunk it up and send it on the queue?
    }

    // TODO: Revisit this when the boxed iterator is cleaned up
    fn visit_boxed_iterator(&mut self, iterator: &MutContainer<OpaqueIterator>) -> Self::Output {
        self.push_back(&iterator.read().root);
    }
    fn visit_boxed_value(&mut self, boxed_value: &MutContainer<SteelVal>) -> Self::Output {
        self.push_back(&boxed_value.read());
    }

    fn visit_closure(&mut self, closure: &ByteCodeLambda) -> Self::Output {
        // for heap_ref in closure.heap_allocated.borrow().iter() {
        //     self.mark_heap_reference(&heap_ref.strong_ptr())
        // }

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
                    self.push_back(&value);
                }

                for value in &continuation.current_frame.function.captures {
                    self.push_back(&value);
                }

                for frame in &continuation.stack_frames {
                    for value in &frame.function.captures {
                        self.push_back(&value);
                    }

                    // if let Some(handler) = &frame.handler {
                    //     self.push_back((*handler.as_ref()).clone());
                    // }

                    if let Some(handler) =
                        frame.attachments.as_ref().and_then(|x| x.handler.clone())
                    {
                        self.push_back(&handler);
                    }
                }
            }

            ContinuationMark::Open(continuation) => {
                for value in &continuation.current_stack_values {
                    self.push_back(value);
                }

                for value in &continuation.current_frame.function.captures {
                    self.push_back(value);
                }
            }
        }
    }
    // TODO: Come back to this
    fn visit_custom_type(
        &mut self,
        custom_type: &'a MutContainer<Box<dyn CustomType>>,
    ) -> Self::Output {
        // todo!()
        custom_type.read().visit_children_ref_queue(self);

        // todo!()
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
