use core::cell::RefCell;
use core::sync::atomic::{AtomicBool, Ordering};
use std::collections::HashSet;

#[cfg(feature = "sync")]
use std::{sync::Arc, thread::JoinHandle};

#[cfg(feature = "sync")]
use std::sync::Mutex;

#[cfg(feature = "sync")]
use crate::rvals::cycles::BreadthFirstSearchSteelValReferenceVisitor2;

#[cfg(feature = "sync")]
use crate::rvals::SteelValPointer;

#[cfg(feature = "sync")]
use crate::gc::shared::MutContainer;
use crate::values::lock::SpinLock;
use crate::{
    compiler::map::SymbolMap,
    gc::{shared::ShareableMut, GcMut},
    rvals::{
        AsRefSteelVal, Custom, IntoSteelVal, OpaqueIterator, RestArgsIter, SteelComplex,
        SteelVector,
    },
    steel_vm::vm::{Continuation, ContinuationMark, Synchronizer, VmCore},
    values::lists::List,
    SteelErr,
};

#[cfg(feature = "sync")]
use crate::steel_vm::vm::apply;

#[cfg(feature = "sync")]
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
    port::SteelPort,
    structs::StructRef,
    transducers::{Reducer, Transducer},
};

#[cfg(feature = "sync")]
use super::lists::Pair;

#[cfg(feature = "sync")]
use super::Vector;

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
        heap.memory_free_list.mark_all_unreachable();
        heap.vector_free_list.mark_all_unreachable();

        // Actually walk the tree, looking for unreachable stuff
        self.visit();

        heap.memory_free_list.recount();
        heap.vector_free_list.recount();

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

impl BreadthFirstSearchSteelValVisitor for GlobalSlotRecycler {
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
                // A flat vector is plain reference counted with no cycle to detect,
                // so walk what it holds
                FlatVector(v) => {
                    for value in v.iter() {
                        self.push_back(value.clone());
                    }
                    self.default_output()
                }
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
                OpCode::CALLGLOBAL
                | OpCode::CALLPRIMITIVE
                | OpCode::PUSH
                | OpCode::CALLGLOBALTAIL
                | OpCode::CALLGLOBALNOARITY
                | OpCode::CALLGLOBALTAILNOARITY => {
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
            stats: MarkAndSweepStats::default(),
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
            stats: MarkAndSweepStats::default(),
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

    fn visit_mutable_vector(&mut self, vector: HeapRef<HeapVec>) -> Self::Output {
        let mut queue = MarkAndSweepContext {
            queue: &mut self.queue,
            stats: MarkAndSweepStats::default(),
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

    fn visit_steel_struct(&mut self, steel_struct: StructRef) -> Self::Output {
        for field in steel_struct.fields().iter() {
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
                crate::values::transducers::Transducers::MapPair(i) => self.push_back(i),
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
// Occupancy after a collection that means the heap has to get bigger
const HEAP_GROW_OCCUPANCY: f64 = 0.5;
// How much bigger the heap gets each time it grows. A full mark happens once per
// growth, so this sets how many marks a run costs
const GC_GROW_FACTOR: usize = 4;
const RESET_LIMIT: usize = 9;

// Units of marking work per free slot left after a collection. Lower means a
// bigger heap and rarer collections, higher means less memory and more of them.
fn gc_work_ratio() -> usize {
    crate::config::config().gc_work_ratio
}

// How much a weak collection has to reclaim, as a fraction of the list it walks,
// to be worth repeating. The scan is O(heap) and reclaims less each time, so a
// nearly full list would otherwise rescan on every allocation. A full collection
// re-arms it.
fn weak_yield_ratio() -> usize {
    crate::config::config().gc_weak_yield
}

// A ceiling on the headroom left after a collection. Each free slot is still a
// live allocation, so without this a large graph would size the heap to gigabytes.
fn gc_max_headroom() -> usize {
    crate::config::config().gc_max_headroom
}

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
    roots: rustc_hash::FxHashMap<(usize, usize), SteelVal>,
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

type HeapElement<T> = steel_rc::weak::Arc<SpinLock<HeapAllocated<T>>>;

// reachable is atomic, so a probe doesn't need to take the slot's lock
fn slot_is_reachable<T: Clone + core::fmt::Debug + PartialEq + Eq>(slot: &HeapElement<T>) -> bool {
    unsafe { slot.get_value().is_reachable() }
}

/// A slot can be handed out again only once nothing points at it. An unreachable
/// cell can still have handles, held by garbage or by something the mark skips
/// like a weak box, and reusing it would show them an unrelated value.
fn slot_is_free<T: Clone + core::fmt::Debug + PartialEq + Eq>(slot: &HeapElement<T>) -> bool {
    !slot_is_reachable(slot) && steel_rc::weak::Arc::weak_count(slot) == 0
}

#[cfg(feature = "sync")]
static MARKER: std::sync::LazyLock<ParallelMarker> = std::sync::LazyLock::new(ParallelMarker::new);

#[cfg(feature = "sync")]
struct Registration {
    value: SteelVal,
    finalizer: SteelVal,
    // Set by a collection that found the value unreachable, for a value that
    // lives on the heap. Any other value is ready once nothing else holds it.
    ready: bool,
}

#[cfg(feature = "sync")]
struct WillState {
    registrations: Mutex<Vec<Registration>>,
    // Signalled when a value is registered or a collection makes one ready.
    wake: Sender<()>,
    woken: Receiver<()>,
}

/// Every live will executor, so a collection can find what they hold.
#[cfg(feature = "sync")]
static WILL_EXECUTORS: Lazy<Mutex<Vec<std::sync::Weak<WillState>>>> =
    Lazy::new(|| Mutex::new(Vec::new()));

/// A will executor, as in Racket. The collector doesn't trace what it holds, so
/// a registered value nothing else reaches becomes ready and its finalizer runs
/// on it. Until then the executor keeps the value, and everything it references,
/// intact.
#[cfg(feature = "sync")]
pub struct WillExecutor {
    state: Arc<WillState>,
}

#[cfg(not(feature = "sync"))]
pub struct WillExecutor {}

#[cfg(feature = "sync")]
impl WillExecutor {
    fn new() -> Self {
        let (wake, woken) = crossbeam_channel::unbounded();
        let state = Arc::new(WillState {
            registrations: Mutex::new(Vec::new()),
            wake,
            woken,
        });

        let mut executors = WILL_EXECUTORS.lock().unwrap();
        executors.retain(|executor| executor.strong_count() > 0);
        executors.push(Arc::downgrade(&state));

        Self { state }
    }

    fn wait(&self) {
        let _ = self.state.woken.recv();
    }

    // Takes the first registration whose value is ready.
    fn find_next(&self) -> Option<Pair> {
        let mut registrations = self.state.registrations.lock().unwrap();
        let index = registrations
            .iter()
            .position(|r| r.ready || only_referenced_here(&r.value))?;
        let registration = registrations.remove(index);
        Some(Pair {
            car: registration.value,
            cdr: registration.finalizer,
        })
    }

    pub fn register(&self, key: SteelVal, func: SteelVal) -> Result<(), SteelErr> {
        log::debug!(target: "will", "Registering: {}", key);
        if !func.is_function() {
            stop!(TypeMismatch => "will-register expects a function, found: {}", func)
        }

        self.state.registrations.lock().unwrap().push(Registration {
            value: key,
            finalizer: func,
            ready: false,
        });
        let _ = self.state.wake.send(());

        Ok(())
    }
}

/// The second half of a mark, as in Racket. A registered value on the heap that
/// the first half didn't reach is ready. Then every registered value and
/// finalizer is marked, so the finalizer sees its value intact. Returns what to
/// mark, holding a reference to each for the duration.
#[cfg(feature = "sync")]
fn will_executor_roots() -> Vec<SteelVal> {
    let mut roots = Vec::new();
    let executors = WILL_EXECUTORS.lock().unwrap();
    for executor in executors.iter().filter_map(|executor| executor.upgrade()) {
        let mut newly_ready = false;
        let mut registrations = executor.registrations.lock().unwrap();
        for registration in registrations.iter_mut() {
            if !registration.ready && heap_value_unreached(&registration.value) {
                registration.ready = true;
                newly_ready = true;
            }
            roots.push(registration.value.clone());
            roots.push(registration.finalizer.clone());
        }
        drop(registrations);

        if newly_ready {
            let _ = executor.wake.send(());
        }
    }
    roots
}

/// Whether `value` is a box or mutable vector the last mark didn't reach.
#[cfg(feature = "sync")]
fn heap_value_unreached(value: &SteelVal) -> bool {
    match value {
        SteelVal::HeapAllocated(heap_ref) => !heap_ref.is_reachable(),
        SteelVal::MutableVector(heap_ref) => !heap_ref.is_reachable(),
        _ => false,
    }
}

/// Whether the one reference to `value` being looked at is the only one.
/// Always false for a value without a reference count.
fn only_referenced_here(value: &SteelVal) -> bool {
    match value {
        SteelVal::Closure(gc) => Gc::strong_count(gc) == 1,
        SteelVal::VectorV(steel_vector) => Gc::strong_count(&steel_vector.0) == 1,
        SteelVal::StringV(steel_string) => Gc::strong_count(&steel_string.0) == 1,
        SteelVal::SymbolV(steel_string) => Gc::strong_count(&steel_string.0) == 1,
        SteelVal::Custom(gc) => Gc::strong_count(gc) == 1,
        SteelVal::HashMapV(steel_hash_map) => Gc::strong_count(&steel_hash_map.0) == 1,
        SteelVal::HashSetV(steel_hash_set) => Gc::strong_count(&steel_hash_set.0) == 1,
        SteelVal::CustomStruct(gc) => gc.strong_count() == 1,
        SteelVal::PortV(steel_port) => Gc::strong_count(&steel_port.port) == 1,
        SteelVal::IterV(gc) => Gc::strong_count(gc) == 1,
        SteelVal::ReducerV(gc) => Gc::strong_count(gc) == 1,
        SteelVal::FutureV(gc) => Gc::strong_count(gc) == 1,
        SteelVal::StreamV(gc) => Gc::strong_count(gc) == 1,
        SteelVal::BoxedFunction(gc) => Gc::strong_count(gc) == 1,
        SteelVal::ContinuationFunction(continuation) => {
            crate::gc::shared::StandardShared::strong_count(&continuation.inner) == 1
        }
        SteelVal::ListV(generic_list) => generic_list.strong_count() == 1,
        SteelVal::Pair(gc) => Gc::strong_count(gc) == 1,
        SteelVal::MutableVector(heap_ref) => steel_rc::weak::Weak::weak_count(&heap_ref.inner) == 1,
        SteelVal::BoxedIterator(gc) => Gc::strong_count(gc) == 1,
        SteelVal::SyntaxObject(gc) => Gc::strong_count(gc) == 1,
        SteelVal::Boxed(gc) => Gc::strong_count(gc) == 1,
        SteelVal::HeapAllocated(heap_ref) => steel_rc::weak::Weak::weak_count(&heap_ref.inner) == 1,
        SteelVal::Reference(gc) => Gc::strong_count(gc) == 1,
        SteelVal::BigNum(gc) => Gc::strong_count(gc) == 1,
        SteelVal::BigRational(gc) => Gc::strong_count(gc) == 1,
        SteelVal::Complex(gc) => Gc::strong_count(gc) == 1,
        SteelVal::ByteVector(steel_byte_vector) => Gc::strong_count(&steel_byte_vector.vec) == 1,
        _ => false,
    }
}

impl Custom for WillExecutor {
    fn into_serializable_steelval(&mut self) -> Option<crate::rvals::SerializableSteelVal> {
        Some(crate::rvals::SerializableSteelVal::Void)
    }
}

#[cfg(feature = "sync")]
#[steel_derive::function(name = "make-will-executor", args(), raises = Never)]
pub fn make_will_executor() -> Result<SteelVal, SteelErr> {
    WillExecutor::new().into_steelval()
}

#[cfg(not(feature = "sync"))]
#[steel_derive::function(name = "make-will-executor", args(), raises = Never)]
pub fn make_will_executor() -> Result<SteelVal, SteelErr> {
    WillExecutor {}.into_steelval()
}

#[cfg(feature = "sync")]
#[steel_derive::context(name = "will-execute", arity = Exact(1))]
pub fn will_execute(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal, SteelErr>> {
    let executor = match WillExecutor::as_ref(&args[0]) {
        Ok(executor) => executor,
        Err(err) => return Some(Err(err)),
    };

    // TODO: Make this find the next thing? Perhaps just have this be a channel
    // or something that waits until the next thing is found?
    // How to do this _with_ blocking? Maybe have a queue for values that are being registered,
    // and then otherwise drain the vector and put them in when found.
    if let SteelVal::Pair(pair) = ctx
        .thread
        .enter_safepoint(|_| -> Result<SteelVal, SteelErr> {
            loop {
                if let Some(value) = executor.find_next() {
                    return Ok(SteelVal::Pair(Gc::new(value)));
                } else {
                    executor.wait();
                }
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

#[cfg(not(feature = "sync"))]
#[steel_derive::context(name = "will-execute", arity = Exact(1))]
pub fn will_execute(_ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal, SteelErr>> {
    if let Err(err) = WillExecutor::as_ref(&args[0]) {
        return Some(Err(err));
    }
    Some(Ok(SteelVal::Void))
}

#[cfg(feature = "sync")]
#[steel_derive::function(name = "will-register", returns = Void)]
pub fn will_register(
    will: SteelVal,
    value: SteelVal,
    func: SteelVal,
) -> Result<SteelVal, SteelErr> {
    let will = WillExecutor::as_ref(&will)?;
    will.register(value, func)?;
    Ok(SteelVal::Void)
}

#[cfg(not(feature = "sync"))]
#[steel_derive::function(
    name = "will-register",
    returns = Void,
    args(Any, Any, Any),
    raises = Never
)]
pub fn will_register(
    _will: SteelVal,
    _value: SteelVal,
    _func: SteelVal,
) -> Result<SteelVal, SteelErr> {
    Ok(SteelVal::Void)
}

/// A weak box, as in Racket. The collector doesn't trace its value, so once
/// nothing else holds the value, `weak-box-value` returns the default.
pub struct WeakBox {
    value: SpinLock<Option<SteelVal>>,
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
#[steel_derive::context(
    name = "make-weak-box",
    arity = Exact(1),
    args(Any),
    raises = Never
)]
pub fn make_weak_box(_ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal, SteelErr>> {
    Some(
        WeakBox {
            value: SpinLock::new(Some(args[0].clone())),
        }
        .into_steelval(),
    )
}

/// Returns the value contained in the weak box.
/// If the garbage collector has proven that the previous content
/// value of weak-box was reachable only through a weak reference,
/// then default-value (which defaults to #f) is returned.
///
/// ```scheme
/// (define value (list 1 2 3))
/// (define weak (make-weak-box value))
/// (weak-box-value weak) ;; => '(1 2 3)
/// (set! value #f) ;; Wipe out the other reference
/// (#%gc-collect)
/// (weak-box-value weak) ;; => #false
/// ```
#[steel_derive::function(name = "weak-box-value")]
pub fn weak_box_value(
    value: &SteelVal,
    mut rest: RestArgsIter<&SteelVal>,
) -> Result<SteelVal, SteelErr> {
    let inner = WeakBox::as_ref(value)?;
    let mut guard = inner.value.lock();

    // A box or mutable vector is alive if the last mark reached it or it was
    // allocated since. Anything else is alive while something besides this
    // weak box holds it.
    let alive = match guard.as_ref() {
        None => false,
        Some(SteelVal::HeapAllocated(heap_ref)) => heap_ref.is_reachable(),
        Some(SteelVal::MutableVector(heap_ref)) => heap_ref.is_reachable(),
        Some(value) => !only_referenced_here(value),
    };

    if alive {
        return Ok(guard.clone().unwrap());
    }

    // Let go of it, so the collector can reuse its slot.
    *guard = None;
    drop(guard);

    Ok(rest
        .next()
        .map(|x| x.unwrap().clone())
        .unwrap_or(SteelVal::BoolV(false)))
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

    #[cfg(feature = "sync")]
    forward: Option<Sender<Vec<HeapElement<T>>>>,
    #[cfg(feature = "sync")]
    backward: Option<Receiver<Vec<HeapElement<T>>>>,

    should_run_weak: bool,
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
                    let guard = x.lock();
                    let inner = guard.value.clone();
                    steel_rc::weak::Arc::new(SpinLock::new(HeapAllocated {
                        reachable: AtomicBool::new(guard.is_reachable()),
                        finalizer: guard.finalizer,
                        value: inner,
                    }))
                })
                .collect(),
            #[cfg(feature = "sync")]
            forward: None,
            #[cfg(feature = "sync")]
            backward: None,

            should_run_weak: self.should_run_weak,
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
        assert!(!var.lock().is_reachable());
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
        assert!(var.lock().is_reachable());
    }

    for var in &free_list.elements[count..] {
        assert!(!var.lock().is_reachable());
    }

    drop(pointers);

    free_list.weak_collection();

    for var in &free_list.elements {
        assert!(!var.lock().is_reachable());
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
        assert!(var.lock().is_reachable());
    }

    for var in &free_list.elements[count..] {
        assert!(!var.lock().is_reachable());
    }

    let right_half = pointers.split_off(100);

    drop(pointers);

    free_list.weak_collection();

    // Check that the first 100 elements are in fact, gone

    for var in &free_list.elements[0..100] {
        assert!(!var.lock().is_reachable());
    }

    drop(right_half)
}

#[inline]
fn run_explicit_merge() {
    #[cfg(feature = "biased")]
    {
        steel_rc::QueueHandle::run_explicit_merge();
    }
}

#[cfg(feature = "sync")]
#[test]
fn parallel_mark_counts_each_slot_exactly_once() {
    use std::sync::{Arc, Barrier};

    let num_slots = 4000;
    let num_threads = 8;

    // Several threads mark the same slots at once, and each slot must only be
    // counted by the thread that set its reachable flag
    let slots: Arc<Vec<HeapElement<SteelVal>>> = Arc::new(
        (0..num_slots)
            .map(|_| steel_rc::weak::Arc::new(SpinLock::new(HeapAllocated::new(SteelVal::Void))))
            .collect(),
    );

    let barrier = Arc::new(Barrier::new(num_threads));

    let handles: Vec<_> = (0..num_threads)
        .map(|_| {
            let slots = Arc::clone(&slots);
            let barrier = Arc::clone(&barrier);
            std::thread::spawn(move || {
                let queue = crossbeam_queue::SegQueue::new();
                let mut local_queue = Vec::new();
                let mut ctx = MarkAndSweepContextRefQueue {
                    local_queue: &mut local_queue,
                    queue: &queue,
                    stats: MarkAndSweepStats::default(),
                };

                barrier.wait();
                for slot in slots.iter() {
                    ctx.mark_heap_reference(slot);
                }

                ctx.stats.memory_reached_count
            })
        })
        .collect();

    let total: usize = handles.into_iter().map(|h| h.join().unwrap()).sum();

    assert_eq!(total, num_slots);
    for slot in slots.iter() {
        assert!(slot.lock().is_reachable());
    }
}

#[cfg(feature = "sync")]
impl<T: HeapAble + Sync + Send + 'static> FreeList<T> {
    // TODO: Calculate the overhead!
    // How big is this?
    const EXTEND_CHUNK: usize = 256 * 100;

    fn new() -> Self {
        // #[cfg(feature = "sync")]
        // let (forward_sender, backward_receiver) = spawn_background_dropper();

        let mut res = FreeList {
            elements: Vec::new(),
            cursor: 0,
            alloc_count: 0,
            grow_count: 0,
            // #[cfg(feature = "sync")]
            // forward: Some(forward_sender),
            // #[cfg(feature = "sync")]
            // backward: Some(backward_receiver),
            forward: None,
            backward: None,
            should_run_weak: true,
        };

        res.grow_by(256);

        res
    }

    // Update the counts for things
    fn recount(&mut self) {
        self.alloc_count = 0;
        for element in &mut self.elements {
            if slot_is_free(element) {
                // Replace with an empty value... for now.
                // Want to keep the memory counts down.
                // let value = core::mem::replace(&mut guard.value, T::empty());
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

    /// `percent_full() > 0.95` without the division, since it is checked on
    /// every allocation: fewer than 1 in 20 slots free.
    fn nearly_full(&self) -> bool {
        self.alloc_count * 20 < self.elements.len()
    }

    fn is_heap_full(&self) -> bool {
        self.alloc_count == 0
    }

    fn grow_by(&mut self, amount: usize) {
        let current = self
            .elements
            .len()
            .saturating_mul(GC_GROW_FACTOR - 1)
            .max(amount);

        self.cursor = self.elements.len();

        self.elements.reserve(current);

        log::debug!(target: "gc", "Time to extend the heap vec");

        // let now = crate::time::Instant::now();

        // Can we pre allocate this somewhere else? Incrementally allocate the values?
        // So basically we can have the elements be allocated vs not, and just have them
        // be snatched on demand?
        self.elements.extend(
            core::iter::repeat_with(|| {
                steel_rc::weak::Arc::new(SpinLock::new(HeapAllocated::new(T::empty())))
            })
            .take(current),
        );

        log::debug!(target: "gc", "Growing the heap by: {}", current);

        self.alloc_count += current;
        self.grow_count += 1;

        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].lock().is_reachable());
        }
    }

    fn grow(&mut self) {
        self.grow_by(Self::EXTEND_CHUNK);
    }

    // Extend the heap
    fn extend_heap(&mut self) {
        self.grow();

        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].lock().is_reachable());
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
        let mut heap_guard = guard.lock();

        // TODO: If the guard is registered with a will executor,
        // then we shouldn't mark this value as eligible to be
        // freed?
        heap_guard.value = value;

        heap_guard.mark_reachable();
        let weak_ptr = steel_rc::weak::Arc::downgrade(guard);
        drop(heap_guard);

        // self.elements[self.cursor] = pointer;
        self.alloc_count -= 1;

        // Find where to assign the next slot optimistically?
        let next_slot = self.elements[self.cursor..]
            .iter()
            .position(|x| slot_is_free(x));

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
                // Move to the beginning, or grow if every unreachable slot
                // still has handles.
                match self.elements.iter().position(|x| slot_is_free(x)) {
                    Some(slot) => self.cursor = slot,
                    None => self.extend_heap(),
                }

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
                let dropped = {
                    let mut guard = x.lock();

                    if !guard.is_reachable() {
                        return;
                    }

                    guard.reset();
                    amount_dropped += 1;
                    core::mem::replace(&mut guard.value, T::empty())
                };

                drop(dropped);
            }
        });

        self.alloc_count += amount_dropped;
        log::debug!(target: "gc", "Free count after weak collection: {}", self.alloc_count);

        amount_dropped
    }

    // Full weak collection
    fn weak_collection(&mut self) -> usize {
        // Just mark them to be dead
        let res = self.collect_on_condition(|inner| steel_rc::weak::Arc::weak_count(inner) == 0);
        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].lock().is_reachable());
        }
        res
    }

    fn mark_all_unreachable(&mut self) {
        self.elements
            .iter()
            .for_each(|x| unsafe { x.get_value().reset() });
    }

    /// After a mark, takes the contents of every unreachable cell. A cell can
    /// still have handles, held by garbage, whose cycles through boxes this
    /// breaks, or by what the mark skips, like weak boxes, which then read
    /// nothing. Its slot comes free once those handles are dropped, and mostly
    /// they are in the contents of other unreachable cells.
    fn take_unreachable(&mut self) -> Vec<T> {
        let mut released = Vec::new();
        for element in &self.elements {
            if slot_is_reachable(element) {
                continue;
            }
            let mut guard = element.lock();
            released.push(core::mem::replace(&mut guard.value, T::empty()));
        }
        released
    }

    // Leaves free slots in proportion to the work the mark just did. Most of
    // what a mark walks lives outside this list, so sizing on occupancy alone
    // would re-trace a large live graph every time a small list fills, which is
    // quadratic in the graph size.
    fn resize_after_collection(&mut self, marking_work: usize) {
        let target_free = (marking_work / gc_work_ratio())
            .max(Self::EXTEND_CHUNK)
            .min(gc_max_headroom());

        if self.alloc_count >= target_free && self.percent_full() <= HEAP_GROW_OCCUPANCY {
            return;
        }

        if self.grow_count > RESET_LIMIT {
            self.compact();
        } else {
            self.grow();
        }
    }

    // Compact every once in a while
    // TODO: Move this on to its own thread
    fn compact(&mut self) {
        #[cfg(feature = "sync")]
        if let Some(sender) = &self.forward {
            sender.send(core::mem::take(&mut self.elements)).unwrap();
            self.elements = self.backward.as_ref().unwrap().recv().unwrap();
        } else {
            self.elements.retain(slot_is_reachable);
            self.elements.shrink_to_fit();
        }

        #[cfg(not(feature = "sync"))]
        {
            self.elements.retain(slot_is_reachable);
            self.elements.shrink_to_fit();
        }

        log::debug!(target: "gc", "Heap size after compaction: {}", self.elements.len());
        self.alloc_count = 0;
        self.grow_count = 0;
        self.extend_heap();
        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].lock().is_reachable());
        }
    }
}

#[cfg(not(feature = "sync"))]
impl<T: HeapAble + 'static> FreeList<T> {
    // TODO: Calculate the overhead!
    // How big is this?
    const EXTEND_CHUNK: usize = 256 * 100;

    fn new() -> Self {
        #[cfg(feature = "sync")]
        let (forward_sender, backward_receiver) = spawn_background_dropper();

        let mut res = FreeList {
            elements: Vec::new(),
            cursor: 0,
            alloc_count: 0,
            grow_count: 0,
            #[cfg(feature = "sync")]
            forward: Some(forward_sender),
            #[cfg(feature = "sync")]
            backward: Some(backward_receiver),
            should_run_weak: true,
        };

        res.grow();

        res
    }

    // Update the counts for things
    fn recount(&mut self) {
        self.alloc_count = 0;
        for element in &mut self.elements {
            if slot_is_free(element) {
                // Replace with an empty value... for now.
                // Want to keep the memory counts down.
                // let value = core::mem::replace(&mut guard.value, T::empty());
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

    /// `percent_full() > 0.95` without the division, since it is checked on
    /// every allocation: fewer than 1 in 20 slots free.
    fn nearly_full(&self) -> bool {
        self.alloc_count * 20 < self.elements.len()
    }

    fn is_heap_full(&self) -> bool {
        self.alloc_count == 0
    }

    fn grow(&mut self) {
        let current = self
            .elements
            .len()
            .saturating_mul(GC_GROW_FACTOR - 1)
            .max(Self::EXTEND_CHUNK);

        self.cursor = self.elements.len();

        self.elements.reserve(current);

        log::debug!(target: "gc", "Time to extend the heap vec");

        // let now = crate::time::Instant::now();

        // Can we pre allocate this somewhere else? Incrementally allocate the values?
        // So basically we can have the elements be allocated vs not, and just have them
        // be snatched on demand?
        self.elements.extend(
            core::iter::repeat_with(|| {
                steel_rc::weak::Arc::new(SpinLock::new(HeapAllocated::new(T::empty())))
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

        heap_guard.mark_reachable();
        let weak_ptr = steel_rc::weak::Arc::downgrade(guard);
        drop(heap_guard);

        // self.elements[self.cursor] = pointer;
        self.alloc_count -= 1;

        // Find where to assign the next slot optimistically?
        let next_slot = self.elements[self.cursor..]
            .iter()
            .position(|x| slot_is_free(x));

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
                // Move to the beginning, or grow if every unreachable slot
                // still has handles.
                match self.elements.iter().position(|x| slot_is_free(x)) {
                    Some(slot) => self.cursor = slot,
                    None => self.extend_heap(),
                }

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
                let dropped = {
                    let mut guard = x.write();

                    if !guard.is_reachable() {
                        return;
                    }

                    guard.reset();
                    amount_dropped += 1;
                    core::mem::replace(&mut guard.value, T::empty())
                };

                drop(dropped);
            }
        });

        self.alloc_count += amount_dropped;
        log::debug!(target: "gc", "Free count after weak collection: {}", self.alloc_count);

        amount_dropped
    }

    // Full weak collection
    fn weak_collection(&mut self) -> usize {
        // Just mark them to be dead
        let res = self.collect_on_condition(|inner| steel_rc::weak::Arc::weak_count(inner) == 0);
        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].read().is_reachable());
        }
        res
    }

    fn mark_all_unreachable(&mut self) {
        self.elements
            .iter()
            .for_each(|x| unsafe { x.get_value().reset() });
    }

    /// After a mark, takes the contents of every unreachable cell. A cell can
    /// still have handles, held by garbage, whose cycles through boxes this
    /// breaks, or by what the mark skips, like weak boxes, which then read
    /// nothing. Its slot comes free once those handles are dropped, and mostly
    /// they are in the contents of other unreachable cells.
    fn take_unreachable(&mut self) -> Vec<T> {
        let mut released = Vec::new();
        for element in &self.elements {
            if slot_is_reachable(element) {
                continue;
            }
            let mut guard = element.write();
            released.push(core::mem::replace(&mut guard.value, T::empty()));
        }
        released
    }

    // Leaves free slots in proportion to the work the mark just did. Most of
    // what a mark walks lives outside this list, so sizing on occupancy alone
    // would re-trace a large live graph every time a small list fills, which is
    // quadratic in the graph size.
    fn resize_after_collection(&mut self, marking_work: usize) {
        let target_free = (marking_work / gc_work_ratio())
            .max(Self::EXTEND_CHUNK)
            .min(gc_max_headroom());

        if self.alloc_count >= target_free && self.percent_full() <= HEAP_GROW_OCCUPANCY {
            return;
        }

        if self.grow_count > RESET_LIMIT {
            self.compact();
        } else {
            self.grow();
        }
    }

    // Compact every once in a while
    // TODO: Move this on to its own thread
    fn compact(&mut self) {
        #[cfg(feature = "sync")]
        if let Some(sender) = &self.forward {
            sender.send(core::mem::take(&mut self.elements)).unwrap();
            self.elements = self.backward.as_ref().unwrap().recv().unwrap();
        } else {
            self.elements.retain(slot_is_reachable);
            self.elements.shrink_to_fit();
        }

        #[cfg(not(feature = "sync"))]
        {
            self.elements.retain(slot_is_reachable);
            self.elements.shrink_to_fit();
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
}

impl FreeList<HeapVec> {
    fn allocate_vec(&mut self, value: impl Iterator<Item = SteelVal>) -> HeapRef<HeapVec> {
        // Drain, moving values around...
        // is that expensive?
        let guard = &mut self.elements[self.cursor];

        // Allocate into this field
        let mut heap_guard = guard.write();

        // Check that this fits:
        heap_guard.value.clear();

        // len is zero after the clear, so this covers the whole iterator.
        // Handing the buffer back is the collector's job, in HeapAble::release
        if let (_, Some(size)) = value.size_hint() {
            heap_guard.value.reserve_exact(size);
        }

        for v in value {
            heap_guard.value.push(v);
        }

        heap_guard.mark_reachable();
        let weak_ptr = steel_rc::weak::Arc::downgrade(guard);
        drop(heap_guard);

        // self.elements[self.cursor] = pointer;
        self.alloc_count -= 1;

        // Find where to assign the next slot optimistically?
        let next_slot = self.elements[self.cursor..]
            .iter()
            .position(|x| slot_is_free(x));

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
                // Move to the beginning, or grow if every unreachable slot
                // still has handles.
                match self.elements.iter().position(|x| slot_is_free(x)) {
                    Some(slot) => self.cursor = slot,
                    None => self.extend_heap(),
                }

                // assert!(!self.elements[self.cursor].read().is_reachable());
            }
        }

        // assert!(!self.elements[self.cursor].read().is_reachable());

        HeapRef { inner: weak_ptr }
    }

    fn calculate_slots_reachable(&self) -> usize {
        let mut amount = 0;
        for element in &self.elements {
            let mut guard = element.write();
            if guard.is_reachable() {
                amount += guard.value.len()
            } else {
                // Eagerly clear the value if its not reachable
                guard.value.clear();
            }
        }

        amount
    }
}

#[cfg(feature = "sync")]
fn spawn_background_dropper<T: HeapAble + Sync + Send + 'static>(
) -> (Sender<Vec<HeapElement<T>>>, Receiver<Vec<HeapElement<T>>>) {
    let (forward_sender, forward_receiver) = crossbeam_channel::bounded(0);
    let (backward_sender, backward_receiver) = crossbeam_channel::bounded(0);

    // Worker thread, capable of dropping the background values of lists
    std::thread::spawn(move || {
        let mut current: Vec<HeapElement<T>> = Vec::new();
        for mut block in forward_receiver {
            core::mem::swap(&mut current, &mut block);
            // Get the value, move on.
            backward_sender.send(block).unwrap();

            // TODO: when compacting, keep values that have been registered with a will
            // executor.
            current.retain(slot_is_reachable);
            current.shrink_to_fit();
        }
    });
    (forward_sender, backward_receiver)
}

/// The heap for steel currently uses an allocation scheme based on weak references
/// to reference counted pointers. Allocation is just a `Vec<Rc<RefCell<T>>>`, where
/// allocating simply pushes and allocates a value at the end. When we do a collection,
/// we attempt to do a small collection by just dropping any values with no weak counts
/// pointing to it.
#[derive(Clone)]
pub struct Heap {
    count: usize,
    mark_and_sweep_queue: Vec<SteelVal>,

    maybe_memory_size: usize,

    skip_minor_collection: bool,
    memory_free_list: FreeList<SteelVal>,
    vector_free_list: FreeList<HeapVec>,
}

#[cfg(feature = "sync")]
unsafe impl Send for Heap {}
#[cfg(feature = "sync")]
unsafe impl Sync for Heap {}

impl Heap {
    pub fn new() -> Self {
        Heap {
            count: 0,
            // mark_and_sweep_queue: VecDeque::with_capacity(256),
            mark_and_sweep_queue: Vec::with_capacity(256),

            maybe_memory_size: 0,

            skip_minor_collection: false,

            memory_free_list: FreeList::new(),
            vector_free_list: FreeList::new(),
        }
    }

    pub fn new_empty() -> Self {
        Heap {
            count: 0,
            mark_and_sweep_queue: Vec::new(),
            maybe_memory_size: 0,
            skip_minor_collection: false,
            memory_free_list: FreeList::new(),
            vector_free_list: FreeList::new(),
        }
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
            core::iter::empty(),
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            force_full,
        );
    }

    pub fn allocate_without_collection(&mut self, value: SteelVal) -> HeapRef<SteelVal> {
        self.memory_free_list.allocate(value)
    }

    pub fn allocate_vec_without_collection(&mut self, value: HeapVec) -> HeapRef<HeapVec> {
        self.vector_free_list.allocate(value)
    }

    // TODO: We can probably fast path the case where we _don't_
    // have an allocation to make. In the event we're not allocating,
    // we don't need to pass all of these values.
    // Clean up the values?
    #[inline(always)]
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
            core::iter::once(value.clone()),
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            false,
        );
        self.memory_free_list.allocate(value)
    }

    /// Allocates a box per value, checking for a collection once for the whole
    /// batch.
    ///
    /// The values are passed to that collection as roots, since none of them is
    /// reachable from a root set while the batch is in flight (the caller has
    /// usually just taken them off the value stack). A collection part way
    /// through would otherwise reclaim the boxes already made, along with
    /// anything only they refer to.
    pub fn allocate_many<'a>(
        &mut self,
        values: &[SteelVal],
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
        out: &mut Vec<SteelVal>,
    ) {
        self.value_collection(
            values.iter().cloned(),
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            false,
        );

        for value in values {
            let allocated = self.memory_free_list.allocate(value.clone());
            out.push(SteelVal::HeapAllocated(allocated));
        }
    }

    /// Like `allocate_many`, for values the caller owns. They are moved into the
    /// boxes instead of cloned. Nothing collects between the collection below
    /// and the moves, so the values only have to be visible to that one call,
    /// which only clones them if a collection runs.
    /// A mutable struct of `descriptor` whose fields are boxes holding
    /// `values`, allocated straight into the struct.
    pub fn allocate_struct_owned<'a>(
        &mut self,
        descriptor: crate::values::structs::StructTypeDescriptor,
        values: smallvec::SmallVec<[SteelVal; 8]>,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> crate::values::structs::StructRef {
        self.value_collection(
            values.iter().cloned(),
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            false,
        );

        let free_list = &mut self.memory_free_list;
        crate::values::structs::StructRef::from_parts(
            descriptor,
            values
                .into_iter()
                .map(|value| SteelVal::HeapAllocated(free_list.allocate(value))),
        )
    }

    pub fn allocate_many_owned<'a>(
        &mut self,
        values: &mut smallvec::SmallVec<[SteelVal; 8]>,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
        out: &mut Vec<SteelVal>,
    ) {
        self.value_collection(
            values.iter().cloned(),
            roots,
            live_functions,
            globals,
            tls,
            synchronizer,
            false,
        );

        for value in values.drain(..) {
            let allocated = self.memory_free_list.allocate(value);
            out.push(SteelVal::HeapAllocated(allocated));
        }
    }

    #[inline(always)]
    fn value_collection<'a>(
        &mut self,
        // The values about to be allocated. They are not reachable from any root
        // set yet, so a collection here has to be told about them explicitly.
        pending: impl Iterator<Item = SteelVal>,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &mut Synchronizer,
        force: bool,
    ) {
        if self.memory_free_list.nearly_full() || force {
            run_explicit_merge();
            // let now = crate::time::Instant::now();
            // Attempt a weak collection
            log::debug!(target: "gc", "SteelVal gc invocation");

            if self.memory_free_list.should_run_weak || force {
                let reclaimed = self.memory_free_list.weak_collection();

                if reclaimed.saturating_mul(weak_yield_ratio())
                    < self.memory_free_list.elements.len()
                {
                    self.memory_free_list.should_run_weak = false;
                }
            }

            log::debug!(target: "gc", "Memory size post weak collection: {}", self.memory_free_list.percent_full());
            // log::debug!(target: "gc", "Weak collection time: {:?}", now.elapsed());

            if self.memory_free_list.nearly_full() || force {
                // New generation
                self.memory_free_list.mark_all_unreachable();
                self.vector_free_list.mark_all_unreachable();

                // Just reset the counter
                let stats = self.mark_and_sweep_new(
                    None,
                    pending,
                    roots,
                    live_functions,
                    globals,
                    tls,
                    synchronizer,
                );

                self.memory_free_list
                    .resize_after_collection(stats.object_count);

                // A full collection changes what is reachable, so the weak scan
                // is worth trying again
                self.memory_free_list.should_run_weak = true;
                self.vector_free_list.should_run_weak = true;

                // synchronizer.resume_threads();

                log::debug!(target: "gc", "Memory size post mark and sweep: {}", self.memory_free_list.percent_full());

                // log::debug!(target: "gc", "---- TOTAL GC TIME: {:?} ----", now.elapsed());
            }
        }
    }

    pub fn allocate_vector<'a>(
        &mut self,
        values: HeapVec,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> HeapRef<HeapVec> {
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
        #[cfg(feature = "biased")]
        {
            steel_rc::QueueHandle::run_explicit_merge();
        }

        if self.vector_free_list.percent_full() > 0.50 && self.vector_free_list.should_run_weak {
            log::debug!(target: "gc", "Running weak collection because the vector free list is 50% full");
            let reclaimed = self.vector_free_list.weak_collection();

            if reclaimed.saturating_mul(weak_yield_ratio()) < self.vector_free_list.elements.len() {
                self.vector_free_list.should_run_weak = false;
            }
        }

        if self.vector_free_list.nearly_full() || force {
            // let now = crate::time::Instant::now();
            // Attempt a weak collection
            log::debug!(target: "gc", "Vec<SteelVal> gc invocation");
            self.vector_free_list.weak_collection();
            self.vector_free_list.calculate_slots_reachable();
            // log::debug!(target: "gc", "Weak collection time: {:?}", now.elapsed());
            // log::debug!(target: "gc", "Weak collection time: {:?}", now.elapsed());

            if self.vector_free_list.nearly_full() || force {
                self.vector_free_list.mark_all_unreachable();
                self.memory_free_list.mark_all_unreachable();

                let stats = self.mark_and_sweep_new(
                    None,
                    values.iter().cloned(),
                    roots,
                    live_functions,
                    globals,
                    tls,
                    synchronizer,
                );

                self.vector_free_list
                    .resize_after_collection(stats.object_count);

                self.vector_free_list.should_run_weak = true;

                log::debug!(target: "gc", "Memory size post mark and sweep: {}", self.vector_free_list.percent_full());

                // log::debug!(target: "gc", "---- TOTAL VECTOR GC TIME: {:?} ----", now.elapsed());
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
    ) -> HeapRef<HeapVec> {
        if self.vector_free_list.percent_full() > 0.50 && self.vector_free_list.should_run_weak {
            run_explicit_merge();
            let reclaimed = self.vector_free_list.weak_collection();

            if reclaimed.saturating_mul(weak_yield_ratio()) < self.vector_free_list.elements.len() {
                self.vector_free_list.should_run_weak = false;
            }
        }

        if self.vector_free_list.nearly_full() {
            run_explicit_merge();
            // let now = crate::time::Instant::now();
            // Attempt a weak collection
            log::debug!(target: "gc", "Vec<SteelVal> gc invocation");
            self.vector_free_list.weak_collection();
            self.vector_free_list.calculate_slots_reachable();
            // log::debug!(target: "gc", "Weak collection time: {:?}", now.elapsed());
            // log::debug!(target: "gc", "Weak collection time: {:?}", now.elapsed());

            if self.vector_free_list.nearly_full() {
                self.vector_free_list.mark_all_unreachable();
                self.memory_free_list.mark_all_unreachable();

                let stats = self.mark_and_sweep_new(
                    None,
                    values.clone(),
                    roots,
                    live_functions,
                    globals,
                    tls,
                    synchronizer,
                );

                self.vector_free_list
                    .resize_after_collection(stats.object_count);

                self.vector_free_list.should_run_weak = true;

                log::debug!(target: "gc", "Memory size post mark and sweep: {}", self.vector_free_list.percent_full());

                // log::debug!(target: "gc", "---- TOTAL VECTOR GC TIME: {:?} ----", now.elapsed());
            }
        }

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

        #[cfg(feature = "profiling")]
        let now = crate::time::Instant::now();

        #[cfg(feature = "sync")]
        {
            GLOBAL_ROOTS.lock().unwrap().increment_generation();
        }

        #[cfg(not(feature = "sync"))]
        {
            ROOTS.with(|x| x.borrow_mut().increment_generation());
        }

        #[cfg(feature = "profiling")]
        log::debug!(target: "gc", "Sweep: Time taken: {:?}", now.elapsed());

        synchronizer.resume_threads();

        // Both lists before dropping anything, since a value in one can hold
        // handles into the other. Dropped outside the locks, then counted.
        let values = self.memory_free_list.take_unreachable();
        let vectors = self.vector_free_list.take_unreachable();
        drop(values);
        drop(vectors);
        self.memory_free_list.recount();
        self.vector_free_list.recount();

        stats

        // object_count.saturating_sub(amount_freed)
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

        #[cfg(feature = "profiling")]
        let now = crate::time::Instant::now();

        let mut context = MarkAndSweepContext {
            queue: &mut self.mark_and_sweep_queue,
            stats: MarkAndSweepStats::default(),
        };

        // A root only needs the address of what it points at, which saves a
        // refcount round trip per global. These are handed over under mark's
        // lock instead of pushed to the shared queue, so a concurrent collection
        // can't drain them
        #[cfg(feature = "sync")]
        let mut root_pointers: Vec<SteelValPointer> = Vec::new();

        macro_rules! push_root {
            ($value:expr) => {{
                #[cfg(feature = "sync")]
                {
                    if let Some(p) = SteelValPointer::from_value($value) {
                        root_pointers.push(p);
                    }
                }

                #[cfg(not(feature = "sync"))]
                context.push_back(($value).clone());
            }};
        }

        // Pause all threads
        synchronizer.stop_threads();
        unsafe {
            synchronizer.enumerate_stacks(&mut context);
        }

        if let Some(root_value) = &root_value {
            push_root!(root_value);
        }

        for value in root_vector {
            push_root!(&value);
        }

        for root in tls {
            push_root!(root);
        }

        log::debug!(target: "gc", "Roots size: {}", roots.len());
        for root in roots {
            push_root!(root);
        }

        log::debug!(target: "gc", "Globals size: {}", globals.len());
        for root in globals {
            push_root!(root);
        }

        for function in function_stack {
            for value in function.captures() {
                push_root!(value);
            }
        }

        #[cfg(feature = "sync")]
        {
            GLOBAL_ROOTS
                .lock()
                .unwrap()
                .roots
                .values()
                .for_each(|value| push_root!(value))
        }

        #[cfg(not(feature = "sync"))]
        {
            ROOTS.with(|x| {
                x.borrow()
                    .roots
                    .values()
                    .for_each(|value| push_root!(value))
            });
        }

        // TODO: Can we do this in parallel? Divide up the iterator into separate components
        // and do them that way?
        log::debug!(target: "gc", "Stack size: {}", context.queue.len());

        // mark only reads the slice and leaves the queue behind, so clear it
        // here or it grows for the life of the process and remarks stale roots
        // on every collection
        #[cfg(feature = "sync")]
        let count = {
            let count = MARKER.mark(context.queue, root_pointers);
            context.queue.clear();

            let wills = will_executor_roots();
            if wills.is_empty() {
                count
            } else {
                let pointers = wills
                    .iter()
                    .filter_map(SteelValPointer::from_value)
                    .collect();
                let more = MARKER.mark(context.queue, pointers);
                context.queue.clear();
                count + more
            }
        };

        #[cfg(not(feature = "sync"))]
        let count = {
            context.visit();
            context.stats
        };

        // log::debug!(target: "gc", "Mark: Time taken: {:?}", now.elapsed());
        count
    }
}

#[cfg(feature = "sync")]
#[derive(Clone)]
struct ParallelMarker {
    // Chunks of the roots for marking
    senders: Arc<Mutex<Vec<MarkerWorker>>>,
    queue: Arc<crossbeam_queue::SegQueue<SteelValPointer>>,
}

#[cfg(feature = "sync")]
unsafe impl Sync for ParallelMarker {}
#[cfg(feature = "sync")]
unsafe impl Send for ParallelMarker {}

#[cfg(feature = "sync")]
struct MarkerWorker {
    // Tell the thread to wake up
    sender: Sender<()>,
    ack: Receiver<MarkAndSweepStats>,
    handle: JoinHandle<()>,
}

#[cfg(feature = "sync")]
impl ParallelMarker {
    pub fn new() -> Self {
        // Marking is memory latency bound, so extra workers mostly add queue
        // contention and cache thrashing. STEEL_GC_MARK_THREADS opts into more.
        let parallelism = crate::config::config().gc_mark_threads.max(1);

        let mut workers = Vec::with_capacity(parallelism);
        let queue = Arc::new(crossbeam_queue::SegQueue::new());

        for _ in 0..parallelism {
            let cloned_queue = queue.clone();
            let (sender, receiver) = crossbeam_channel::unbounded();
            let (ack_sender, ack_receiver) = crossbeam_channel::unbounded();

            let handle = std::thread::spawn(move || {
                let cloned_queue = cloned_queue;
                // This... might be too big?
                let mut local_queue = Vec::with_capacity(4096);
                for _ in receiver {
                    let now = crate::time::Instant::now();

                    let mut context = MarkAndSweepContextRefQueue {
                        queue: &cloned_queue,
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

    pub fn mark(&self, queue: &[SteelVal], roots: Vec<SteelValPointer>) -> MarkAndSweepStats {
        let guard = self.senders.lock().unwrap();

        for value in queue.iter() {
            if let Some(p) = SteelValPointer::from_value(value) {
                // Start with a local queue first?
                self.queue.push(p);
            }
        }

        for root in roots {
            self.queue.push(root);
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

pub trait HeapAble: Clone + core::fmt::Debug + PartialEq + Eq {
    fn empty() -> Self;

    /// Drops a value taken out of a box whose last handle went away, without
    /// recursing through whatever it holds. Otherwise a chain of boxes, such as
    /// a mutable list built from mutable structs, drops one box per native frame.
    fn release(self);
}
impl HeapAble for SteelVal {
    fn empty() -> Self {
        SteelVal::Void
    }

    fn release(self) {
        if !needs_release(&self) {
            return;
        }
        // A box's value is usually a struct or a list. Dropping it in place
        // skips the work list, and a chain of them, like a list of mutable
        // pairs, only nests so far before the rest goes through the list.
        if box_release_direct_enabled() {
            release_bounded(self, |value| {
                let _ = crate::rvals::cycles::drop_fields_iteratively(core::iter::once(value));
            });
        } else {
            let _ = crate::rvals::cycles::drop_fields_iteratively(core::iter::once(self));
        }
    }
}

/// `STEEL_BOX_RELEASE_DIRECT=0` queues a released box's contents on the drop
/// work list instead of dropping them in place.
fn box_release_direct_enabled() -> bool {
    crate::config::config().box_release_direct
}

// How many releases may nest on the native stack before the rest go through
// the iterative work list.
const RELEASE_DIRECT_DEPTH: u32 = 32;

thread_local! {
    static RELEASE_DEPTH: core::cell::Cell<u32> = const { core::cell::Cell::new(0) };
}

/// Drops `value` in place while releases are shallowly nested, and hands it to
/// `queue` past that. Nearly every release is a box or small vector that holds
/// nothing that nests, and the work list is expensive for those. Only a chain of
/// boxes, each the last handle to the next, nests deeply, and that switches to
/// the work list after `RELEASE_DIRECT_DEPTH`.
#[inline]
fn release_bounded<V>(value: V, queue: impl FnOnce(V)) {
    let direct = RELEASE_DEPTH
        .try_with(|depth| {
            let current = depth.get();
            if current < RELEASE_DIRECT_DEPTH {
                depth.set(current + 1);
                true
            } else {
                false
            }
        })
        .unwrap_or(false);

    if direct {
        drop(value);
        let _ = RELEASE_DEPTH.try_with(|depth| depth.set(depth.get() - 1));
    } else {
        queue(value);
    }
}

/// Whether dropping the value does work worth routing through the work list.
#[inline]
fn needs_release(value: &SteelVal) -> bool {
    !matches!(
        value,
        SteelVal::BoolV(_)
            | SteelVal::NumV(_)
            | SteelVal::IntV(_)
            | SteelVal::CharV(_)
            | SteelVal::Void
            | SteelVal::FuncV(_)
            | SteelVal::MutFunc(_)
            | SteelVal::BuiltIn(_)
    )
}

// Backing store for SteelVal::MutableVector. steel_vec is repr(C) with const
// offset accessors, which is what lets the jit address elements inline
pub type HeapVec = steel_vec::Vec<SteelVal>;

impl HeapAble for HeapVec {
    fn empty() -> Self {
        Self::new()
    }

    fn release(self) {
        // Only values that can hold more structure go through the work list. A
        // vector of fixnums would otherwise go through the queue one element at
        // a time, where dropping the buffer directly is free.
        if !self.iter().any(needs_release) {
            return;
        }
        release_bounded(self, |vector| {
            let _ = crate::rvals::cycles::drop_fields_iteratively(
                vector.into_iter().filter(needs_release),
            );
        });
    }
}

// The jit hands HeapRef straight to drop-box and drop-boxed-vec as a bare
// pointer, so the single field layout has to be guaranteed
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct HeapRef<T: HeapAble> {
    // TODO: Replace the inner value with a spin lock?
    // Shouldn't really be possible to have this be an issue
    pub(crate) inner: steel_rc::weak::Weak<SpinLock<HeapAllocated<T>>>,
}

/// A box can only be read through a handle, so once the last handle is gone
/// nothing can observe its value again, and the value is released right away.
/// The heap decides when to collect by how many boxes it holds and not by what
/// they keep alive, so a few boxes each pinning a large structure might
/// otherwise never be collected. The slot itself is still reclaimed by the heap.
///
/// Cycles through boxes keep their handles alive and are left to mark and
/// sweep.
impl<T: HeapAble> Drop for HeapRef<T> {
    #[inline]
    fn drop(&mut self) {
        if let Some(value) = self.take_if_last() {
            value.release();
        }
    }
}

impl<T: HeapAble> HeapRef<T> {
    /// Takes the value out when this is the only handle left, leaving the
    /// box empty.
    #[inline]
    pub(crate) fn take_if_last(&self) -> Option<T> {
        if self.inner.weak_count() != 1 {
            return None;
        }
        let inner = self.inner.upgrade()?;
        // Only if the lock is free. The collector emptying a dead slot and `set`
        // drop a box's old value while holding that box's lock, and when the
        // value cycles back through the same box (a closure capturing it), the
        // drop lands here on a lock this thread already holds. The spin lock
        // isn't reentrant, so waiting would spin forever. The holder is
        // replacing the value anyway, and anything else is left to the
        // collector.
        let mut guard = inner.try_lock()?;
        Some(core::mem::replace(&mut guard.value, T::empty()))
    }

    #[inline(always)]
    pub fn get(&self) -> T {
        self.inner.upgrade().unwrap().lock().value.clone()
    }

    /// Whether the last mark reached this, or it was allocated since.
    pub(crate) fn is_reachable(&self) -> bool {
        self.inner
            .upgrade()
            .is_some_and(|inner| inner.lock().is_reachable())
    }

    pub unsafe fn inner_ref(&self) -> Option<&T> {
        if self.inner.weak_count() == 1 {
            Some(&(&*self.inner.as_ptr()).get_value().value)
        } else {
            None
        }
    }

    pub fn borrow<O>(&self, thunk: impl FnOnce(&T) -> O) -> O {
        let value = self.inner.upgrade().unwrap();
        let value = value.lock();
        thunk(&value.value)
    }

    pub fn as_ptr_usize(&self) -> usize {
        self.inner.as_ptr() as usize
    }

    pub fn set(&mut self, value: T) -> T {
        let inner = self.inner.upgrade().unwrap();

        let ret = { inner.lock().value.clone() };

        inner.write().value = value;
        ret
    }

    pub fn set_and_return(&self, value: T) -> T {
        let inner = self.inner.upgrade().unwrap();

        let mut guard = inner.write();
        core::mem::replace(&mut guard.value, value)
    }

    pub(crate) fn set_interior_mut(&self, value: T) -> T {
        let inner = self.inner.upgrade().unwrap();

        let ret = { inner.lock().value.clone() };

        inner.write().value = value;
        ret
    }

    pub(crate) fn strong_ptr(&self) -> steel_rc::weak::Arc<SpinLock<HeapAllocated<T>>> {
        self.inner.upgrade().unwrap()
    }

    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        steel_rc::weak::Weak::ptr_eq(&self.inner, &other.inner)
    }
}

// reachable is atomic so the allocator and the marker can read and set it
// without taking the spin lock
#[derive(Debug)]
pub struct HeapAllocated<T: Clone + core::fmt::Debug + PartialEq + Eq> {
    pub(crate) reachable: AtomicBool,
    pub(crate) finalizer: bool,
    pub(crate) value: T,
}

impl<T: Clone + core::fmt::Debug + PartialEq + Eq> Clone for HeapAllocated<T> {
    fn clone(&self) -> Self {
        Self {
            reachable: AtomicBool::new(self.is_reachable()),
            finalizer: self.finalizer,
            value: self.value.clone(),
        }
    }
}

impl<T: Clone + core::fmt::Debug + PartialEq + Eq> PartialEq for HeapAllocated<T> {
    fn eq(&self, other: &Self) -> bool {
        self.is_reachable() == other.is_reachable()
            && self.finalizer == other.finalizer
            && self.value == other.value
    }
}

impl<T: Clone + core::fmt::Debug + PartialEq + Eq> Eq for HeapAllocated<T> {}

#[test]
fn check_size_of_heap_allocated_value() {
    println!("{:?}", core::mem::size_of::<HeapAllocated<SteelVal>>());
}

impl<T: Clone + core::fmt::Debug + PartialEq + Eq> HeapAllocated<T> {
    pub fn new(value: T) -> Self {
        Self {
            reachable: AtomicBool::new(false),
            finalizer: false,
            value,
        }
    }

    pub fn is_reachable(&self) -> bool {
        self.reachable.load(Ordering::Relaxed)
    }

    pub(crate) fn mark_reachable(&self) {
        self.reachable.store(true, Ordering::Relaxed);
    }

    // Marks and reports whether it was already marked, so the marker can test
    // and set in one step
    pub(crate) fn mark_reachable_was_set(&self) -> bool {
        self.reachable.swap(true, Ordering::Relaxed)
    }

    pub(crate) fn reset(&self) {
        self.reachable.store(false, Ordering::Relaxed);
    }
}

pub struct MarkAndSweepContext<'a> {
    queue: &'a mut Vec<SteelVal>,
    stats: MarkAndSweepStats,
}

impl<'a> MarkAndSweepContext<'a> {
    pub(crate) fn mark_heap_reference(
        &mut self,
        heap_ref: &steel_rc::weak::Arc<SpinLock<HeapAllocated<SteelVal>>>,
    ) {
        if unsafe { heap_ref.get_value().mark_reachable_was_set() } {
            return;
        }

        self.stats.memory_reached_count += 1;

        self.push_back(heap_ref.lock().value.clone());
    }

    // Visit the heap vector, mark it as visited!
    pub(crate) fn mark_heap_vector(
        &mut self,
        heap_vector: &steel_rc::weak::Arc<SpinLock<HeapAllocated<HeapVec>>>,
    ) {
        if unsafe { heap_vector.get_value().mark_reachable_was_set() } {
            return;
        }

        self.stats.vector_reached_count += 1;

        for value in heap_vector.lock().value.iter() {
            self.push_back(value.clone());
        }
    }
}

#[derive(Debug, Clone, Default)]
struct MarkAndSweepStats {
    object_count: usize,
    memory_reached_count: usize,
    vector_reached_count: usize,
    keep_alive: Vec<SteelVal>,
}

impl core::ops::Add for MarkAndSweepStats {
    type Output = MarkAndSweepStats;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.object_count += rhs.object_count;
        self.memory_reached_count += rhs.memory_reached_count;
        self.vector_reached_count += rhs.vector_reached_count;
        self
    }
}

#[cfg(feature = "sync")]
pub struct MarkAndSweepContextRefQueue<'a> {
    // Thread local queue + larger queue when it runs out?
    // Try to push on to local queue first.
    local_queue: &'a mut Vec<SteelValPointer>,
    queue: &'a crossbeam_queue::SegQueue<SteelValPointer>,
    stats: MarkAndSweepStats,
}

#[cfg(feature = "sync")]
impl<'a> MarkAndSweepContextRefQueue<'a> {
    pub(crate) fn mark_heap_reference(
        &mut self,
        heap_ref: &steel_rc::weak::Arc<SpinLock<HeapAllocated<SteelVal>>>,
    ) {
        if unsafe { heap_ref.get_value().mark_reachable_was_set() } {
            return;
        }

        self.stats.memory_reached_count += 1;

        self.push_back(&heap_ref.lock().value);
    }

    // Visit the heap vector, mark it as visited!
    pub(crate) fn mark_heap_vector(
        &mut self,
        heap_vector: &steel_rc::weak::Arc<SpinLock<HeapAllocated<HeapVec>>>,
    ) {
        if unsafe { heap_vector.get_value().mark_reachable_was_set() } {
            return;
        }

        self.stats.vector_reached_count += 1;

        for value in heap_vector.lock().value.iter() {
            self.push_back(value);
        }
    }

    pub(crate) fn save(&mut self, value: SteelVal) {
        self.stats.keep_alive.push(value);
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
        self.stats.object_count += 1;

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
            | SteelVal::BigNum(_) => (),
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

            ContinuationMark::Open(_) => {
                /*
                for value in &continuation.current_stack_values {
                    self.push_back(value.clone());
                }

                for value in &continuation.current_frame.function.captures {
                    self.push_back(value.clone());
                }
                */
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

    fn visit_mutable_vector(&mut self, vector: HeapRef<HeapVec>) -> Self::Output {
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

    fn visit_steel_struct(&mut self, steel_struct: StructRef) -> Self::Output {
        for field in steel_struct.fields().iter() {
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
                crate::values::transducers::Transducers::MapPair(i) => self.push_back(i),
            }
        }
    }

    fn visit_void(&mut self) -> Self::Output {}

    fn visit_pair(&mut self, pair: Gc<super::lists::Pair>) -> Self::Output {
        self.push_back(pair.car());
        self.push_back(pair.cdr());
    }
}

#[cfg(feature = "sync")]
impl<'a> BreadthFirstSearchSteelValReferenceVisitor2<'a> for MarkAndSweepContextRefQueue<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

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
            | SteelVal::BigNum(_) => (),
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
            self.push_back(capture);
        }

        if let Some(contract) = closure.get_contract_information().as_ref() {
            self.push_back(contract);
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
                    self.push_back(value);
                }

                for value in &continuation.current_frame.function.captures {
                    self.save(value.clone());
                    self.push_back(value);
                }

                for frame in &continuation.stack_frames {
                    for value in &frame.function.captures {
                        self.save(value.clone());
                        self.push_back(value);
                    }

                    if let Some(handler) =
                        frame.attachments.as_ref().and_then(|x| x.handler.clone())
                    {
                        self.save(handler.clone());
                        self.push_back(&handler);
                    }
                }
            }

            ContinuationMark::Open(_) => {
                /*
                for value in &continuation.current_stack_values {
                    self.save(value.clone());
                    self.push_back(value);
                }

                for value in &continuation.current_frame.function.captures {
                    self.save(value.clone());
                    self.push_back(value);
                }
                */
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
            stats: MarkAndSweepStats::default(),
        };

        // Intercept the downstream ones, keep them alive by using
        // a local queue first, and then spilling over to the larger one.
        custom_type.read().visit_children(&mut temporary_queue);

        self.stats.memory_reached_count += temporary_queue.stats.memory_reached_count;
        self.stats.object_count += temporary_queue.stats.object_count;
        self.stats.vector_reached_count += temporary_queue.stats.vector_reached_count;

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

    fn visit_mutable_vector(&mut self, vector: HeapRef<HeapVec>) -> Self::Output {
        self.mark_heap_vector(&vector.strong_ptr())
    }

    fn visit_reducer(&mut self, reducer: &Reducer) -> Self::Output {
        match reducer {
            Reducer::ForEach(f) => self.push_back(f),
            Reducer::Generic(rf) => {
                self.push_back(&rf.initial_value);
                self.push_back(&rf.function);
            }
            _ => {}
        }
    }

    fn visit_steel_struct(&mut self, fields: &[SteelVal]) -> Self::Output {
        for field in fields.iter() {
            self.push_back(field);
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
                crate::values::transducers::Transducers::MapPair(i) => self.push_back(i),
            }
        }
    }

    fn visit_pair(&mut self, pair: &super::lists::Pair) -> Self::Output {
        self.push_back(pair.car_ref());
        self.push_back(pair.cdr_ref());
    }
}
