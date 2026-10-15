use crate::steel_vm::vm::SteelThread;
use core::cell::{Cell, RefCell};
use core::sync::atomic::{AtomicBool, Ordering};
use std::collections::HashSet;

#[cfg(feature = "sync")]
use std::{sync::Arc, thread::JoinHandle};

#[cfg(feature = "sync")]
use std::sync::{Condvar, Mutex, MutexGuard};

#[cfg(feature = "sync")]
use std::collections::VecDeque;

#[cfg(feature = "sync")]
use core::sync::atomic::AtomicUsize;

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
    port::SteelPort,
    structs::{
        MutableStructCell, MutableStructRef, StructRef, StructTypeDescriptor, MUTABLE_STRUCT_DEAD,
        MUTABLE_STRUCT_LIVE, MUTABLE_STRUCT_SPARE,
    },
    transducers::{Reducer, Transducer},
};

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
        heap.mutable_structs.mark_all_unreachable();

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
                MutableStruct(c) => self.visit_mutable_struct(c),
                PortV(p) => self.visit_port(p),
                IterV(t) => self.visit_transducer(t),
                ReducerV(r) => self.visit_reducer(r),
                FutureFunc(f) => self.visit_future_function(f),
                FutureV(f) => self.visit_future(f),
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
            #[cfg(feature = "sync")]
            watched: None,
        };

        visit_custom_children(&custom_type, &mut queue);
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
            #[cfg(feature = "sync")]
            watched: None,
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
            #[cfg(feature = "sync")]
            watched: None,
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

    // The reachable flag keeps a struct from being walked twice, which is
    // also what ends the walk on a cycle.
    fn visit_mutable_struct(&mut self, steel_struct: MutableStructRef) -> Self::Output {
        let cell = steel_struct.cell();
        if cell.mark_reachable_was_set() {
            return;
        }

        for field in unsafe { cell.fields_unlocked() } {
            self.push_back(field.clone());
        }
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
}

#[cfg(feature = "sync")]
#[derive(Default)]
struct Wills {
    // Registered, and reachable as far as any collection has found.
    waiting: Vec<Registration>,
    // Found unreachable by a collection, in the order they were found.
    // `will-execute` takes from the front.
    ready: VecDeque<Registration>,
}

#[cfg(feature = "sync")]
struct WillState {
    // The heap of the engine that made this, by its address. A collection
    // only has an answer for values on its own heap, and an engine only
    // collects its own.
    heap: usize,
    wills: Mutex<Wills>,
    // Signalled when a collection makes a will ready.
    readied: Condvar,
}

#[cfg(feature = "sync")]
impl WillState {
    fn lock(&self) -> MutexGuard<'_, Wills> {
        self.wills.lock().unwrap_or_else(|e| e.into_inner())
    }
}

/// Every live will executor, so a collection can find what they hold.
#[cfg(feature = "sync")]
static WILL_EXECUTORS: Lazy<Mutex<Vec<std::sync::Weak<WillState>>>> =
    Lazy::new(|| Mutex::new(Vec::new()));

/// A will executor, as in Racket. A registered value becomes ready at the
/// collection that finds nothing else reaching it, and its will is then run
/// on it by `will-execute`. Until then the executor keeps the value, and
/// everything it references, intact.
#[cfg(feature = "sync")]
pub struct WillExecutor {
    state: Arc<WillState>,
}

#[cfg(not(feature = "sync"))]
pub struct WillExecutor {}

#[cfg(feature = "sync")]
impl WillExecutor {
    fn new(heap: usize) -> Self {
        let state = Arc::new(WillState {
            heap,
            wills: Mutex::new(Wills::default()),
            readied: Condvar::new(),
        });

        let mut executors = WILL_EXECUTORS.lock().unwrap();
        executors.retain(|executor| executor.strong_count() > 0);
        executors.push(Arc::downgrade(&state));

        Self { state }
    }

    // Blocks until a collection has made a will ready.
    fn wait_until_ready(&self) {
        let mut wills = self.state.lock();
        while wills.ready.is_empty() {
            wills = self
                .state
                .readied
                .wait(wills)
                .unwrap_or_else(|e| e.into_inner());
        }
    }

    // Takes the will that has been ready the longest.
    fn take_ready(&self) -> Option<Registration> {
        self.state.lock().ready.pop_front()
    }

    pub fn register(&self, key: SteelVal, func: SteelVal) -> Result<(), SteelErr> {
        log::debug!(target: "will", "Registering: {}", key);
        if !func.is_function() {
            stop!(TypeMismatch => "will-register expects a function, found: {}", func)
        }

        self.state.lock().waiting.push(Registration {
            value: key,
            finalizer: func,
        });

        Ok(())
    }
}

/// The will executors made on `heap`.
#[cfg(feature = "sync")]
fn will_executors_on(heap: usize) -> Vec<Arc<WillState>> {
    let mut executors = WILL_EXECUTORS.lock().unwrap_or_else(|e| e.into_inner());
    executors.retain(|executor| executor.strong_count() > 0);
    executors
        .iter()
        .filter_map(|executor| executor.upgrade())
        .filter(|executor| executor.heap == heap)
        .collect()
}

// Held by a collection from before it marks until it has decided which wills
// are ready, so that two heaps collecting at once don't read each other's
// answers.
#[cfg(feature = "sync")]
static WILL_TURN: Mutex<()> = Mutex::new(());

// The registered values a mark under way is looking out for.
#[cfg(feature = "sync")]
static WATCHED: Mutex<Option<Arc<Watched>>> = Mutex::new(None);

// Whether `WATCHED` holds anything, so a mark with no wills to decide
// doesn't take the lock.
#[cfg(feature = "sync")]
static WATCHING: AtomicBool = AtomicBool::new(false);

/// The reference counted values that are registered and waiting, each with
/// whether the mark has come across it. A mark records what it reaches only
/// for boxes, mutable vectors and mutable structs, so this is how it says
/// that it reached one of these.
///
/// The marker passes over the kinds in `unwatched` without looking, so one
/// of those is only noted when it is a root.
#[cfg(feature = "sync")]
struct Watched {
    reached: rustc_hash::FxHashMap<usize, AtomicBool>,
}

#[cfg(feature = "sync")]
impl Watched {
    // Kept out of line, so the marker's loops stay as small as they were.
    #[inline(never)]
    fn note(&self, value: &SteelVal) {
        if let Some(reached) = counted_address(value).and_then(|a| self.reached.get(&a)) {
            reached.store(true, Ordering::Relaxed);
        }
    }

    fn was_reached(&self, address: usize) -> bool {
        self.reached
            .get(&address)
            .is_some_and(|reached| reached.load(Ordering::Relaxed))
    }
}

/// What the mark under way is looking out for, if anything.
#[cfg(feature = "sync")]
#[inline]
fn watched() -> Option<Arc<Watched>> {
    if !WATCHING.load(Ordering::SeqCst) {
        return None;
    }
    WATCHED.lock().unwrap_or_else(|e| e.into_inner()).clone()
}

/// The part will executors have in a collection of their heap, as in Racket.
///
/// Before the mark, every will procedure and every value that is already
/// ready is made a root, since the executor holds those for real. The mark
/// then says which waiting values something still reaches. For a box, a
/// mutable vector or a mutable struct that is its usual flag, and for a
/// reference counted value it is `Watched`.
///
/// After the mark, `settle` looks at the waiting values the mark didn't
/// reach. One of those is ready unless a reference to it is left unaccounted
/// for, which is taken to be something in use that the mark doesn't trace,
/// such as a value held by native code. They are all walked together, so
/// values that only each other reach are all ready at once.
#[cfg(feature = "sync")]
struct WillCollection<'a> {
    heap: usize,
    executors: &'a [Arc<WillState>],
    // In the order of `executors`. Held throughout, so the roots handed to
    // the mark stay where they are.
    wills: Vec<MutexGuard<'a, Wills>>,
    watched: Option<Arc<Watched>>,
    _turn: MutexGuard<'static, ()>,
}

#[cfg(feature = "sync")]
impl<'a> WillCollection<'a> {
    /// None if there is nothing registered on this heap. Called with every
    /// other thread stopped, which is what makes holding the locks safe: a
    /// thread that is stopped inside a primitive can be waiting for one, but
    /// none is waiting for this thread while holding one.
    fn begin(heap: usize, executors: &'a [Arc<WillState>]) -> Option<Self> {
        if executors.is_empty() {
            return None;
        }

        let turn = WILL_TURN.lock().unwrap_or_else(|e| e.into_inner());
        let wills: Vec<_> = executors.iter().map(|executor| executor.lock()).collect();
        if wills
            .iter()
            .all(|wills| wills.waiting.is_empty() && wills.ready.is_empty())
        {
            return None;
        }

        let reached: rustc_hash::FxHashMap<usize, AtomicBool> = wills
            .iter()
            .flat_map(|wills| wills.waiting.iter())
            .filter_map(|registration| counted_address(&registration.value))
            .map(|address| (address, AtomicBool::new(false)))
            .collect();

        let watched = (!reached.is_empty()).then(|| Arc::new(Watched { reached }));
        if watched.is_some() {
            *WATCHED.lock().unwrap_or_else(|e| e.into_inner()) = watched.clone();
            WATCHING.store(true, Ordering::SeqCst);
        }

        Some(Self {
            heap,
            executors,
            wills,
            watched,
            _turn: turn,
        })
    }

    /// What the executors hold for real.
    fn roots(&self) -> impl Iterator<Item = &SteelVal> {
        self.wills.iter().flat_map(|wills| {
            let waiting = wills.waiting.iter().map(|r| &r.finalizer);
            let ready = wills.ready.iter().flat_map(|r| [&r.value, &r.finalizer]);
            waiting.chain(ready)
        })
    }

    /// Moves the wills whose values the mark shows to be unreachable over to
    /// ready, and marks what the waiting values hold, so that a will is
    /// handed its value intact. Returns what that came to, and the values
    /// left for the marker to go through.
    fn settle(mut self) -> (MarkAndSweepStats, Vec<SteelVal>) {
        if self.watched.is_some() {
            WATCHING.store(false, Ordering::SeqCst);
            *WATCHED.lock().unwrap_or_else(|e| e.into_inner()) = None;
        }

        let mut walk = UnreachedWalk::default();

        // The node of each waiting value the mark didn't reach.
        let mut nodes: Vec<Vec<Option<u32>>> = Vec::with_capacity(self.wills.len());
        for wills in &self.wills {
            let of_these = wills
                .waiting
                .iter()
                .map(|registration| {
                    let value = &registration.value;
                    let unreached = match counted_address(value) {
                        Some(address) => !self
                            .watched
                            .as_ref()
                            .is_some_and(|watched| watched.was_reached(address)),
                        None => heap_value_unreached(value),
                    };
                    if unreached {
                        walk.node(value.clone())
                    } else {
                        None
                    }
                })
                .collect();
            nodes.push(of_these);
        }

        walk.visit();
        walk.close();

        let live = walk.held_from_outside(&self.registered());

        for ((wills, nodes), executor) in self.wills.iter_mut().zip(nodes).zip(self.executors) {
            let waiting = core::mem::take(&mut wills.waiting);
            let before = wills.ready.len();
            for (registration, node) in waiting.into_iter().zip(nodes) {
                if node.is_some_and(|node| !live[node as usize]) {
                    wills.ready.push_back(registration);
                } else {
                    wills.waiting.push(registration);
                }
            }
            if wills.ready.len() != before {
                executor.readied.notify_all();
            }
        }

        let stats = MarkAndSweepStats {
            object_count: walk.nodes.len(),
            memory_reached_count: walk.cells_reached,
            vector_reached_count: walk.vectors_reached,
            ..Default::default()
        };
        (stats, walk.opaque)
    }

    /// How many waiting registrations hold each reference counted value, on
    /// any heap. A registration has a reference of its own, which is not one
    /// that keeps the value from being ready.
    fn registered(&self) -> rustc_hash::FxHashMap<usize, usize> {
        let mut registered = rustc_hash::FxHashMap::default();
        let mut count = |wills: &Wills| {
            for address in wills
                .waiting
                .iter()
                .filter_map(|registration| counted_address(&registration.value))
            {
                *registered.entry(address).or_insert(0) += 1;
            }
        };

        for wills in &self.wills {
            count(wills);
        }

        let elsewhere: Vec<Arc<WillState>> = WILL_EXECUTORS
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .iter()
            .filter_map(|executor| executor.upgrade())
            .filter(|executor| executor.heap != self.heap)
            .collect();
        for executor in elsewhere {
            count(&executor.lock());
        }

        registered
    }
}

/// Whether `value` is a box, a mutable vector or a mutable struct that the
/// last mark didn't reach.
#[cfg(feature = "sync")]
fn heap_value_unreached(value: &SteelVal) -> bool {
    match value {
        SteelVal::HeapAllocated(heap_ref) => !heap_ref.is_reachable(),
        SteelVal::MutableVector(heap_ref) => !heap_ref.is_reachable(),
        SteelVal::MutableStruct(s) => !s.is_reachable(),
        _ => false,
    }
}

/// Where a reference counted value lives, which tells it apart from every
/// other value. None for a value with no count, and for a box, a mutable
/// vector or a mutable struct, which a mark has the answer for.
#[cfg(feature = "sync")]
fn counted_address(value: &SteelVal) -> Option<usize> {
    Some(match value {
        SteelVal::Closure(gc) => gc.as_ptr() as usize,
        SteelVal::VectorV(vector) => vector.0.as_ptr() as usize,
        SteelVal::StringV(string) => string.0.as_ptr() as usize,
        SteelVal::SymbolV(string) => string.0.as_ptr() as usize,
        SteelVal::Custom(gc) => gc.as_ptr() as *const u8 as usize,
        SteelVal::HashMapV(map) => map.0.as_ptr() as usize,
        SteelVal::HashSetV(set) => set.0.as_ptr() as usize,
        SteelVal::CustomStruct(s) => s.as_ptr() as usize,
        SteelVal::PortV(port) => port.port.as_ptr() as *const u8 as usize,
        SteelVal::IterV(gc) => gc.as_ptr() as usize,
        SteelVal::ReducerV(gc) => gc.as_ptr() as usize,
        SteelVal::FutureV(gc) => gc.as_ptr() as *const u8 as usize,
        SteelVal::BoxedFunction(gc) => gc.as_ptr() as *const u8 as usize,
        SteelVal::ContinuationFunction(continuation) => {
            crate::gc::shared::StandardShared::as_ptr(&continuation.inner) as *const u8 as usize
        }
        SteelVal::ListV(list) => list.as_ptr_usize(),
        SteelVal::Pair(gc) => gc.as_ptr() as usize,
        SteelVal::BoxedIterator(gc) => gc.as_ptr() as *const u8 as usize,
        SteelVal::SyntaxObject(gc) => gc.as_ptr() as usize,
        SteelVal::Boxed(gc) => gc.as_ptr() as *const u8 as usize,
        SteelVal::Reference(gc) => gc.as_ptr() as *const u8 as usize,
        SteelVal::BigNum(gc) => gc.as_ptr() as usize,
        SteelVal::BigRational(gc) => gc.as_ptr() as usize,
        SteelVal::Complex(gc) => gc.as_ptr() as usize,
        SteelVal::ByteVector(bytes) => bytes.vec.as_ptr() as *const u8 as usize,
        SteelVal::FlatVector(gc) => gc.as_ptr() as usize,
        _ => return None,
    })
}

/// How many references there are to a value that `counted_address` has an
/// answer for, the one being looked at included. Never less than there are.
fn reference_count(value: &SteelVal) -> Option<usize> {
    Some(match value {
        SteelVal::Closure(gc) => Gc::strong_count(gc),
        SteelVal::VectorV(vector) => Gc::strong_count(&vector.0),
        SteelVal::StringV(string) => Gc::strong_count(&string.0),
        SteelVal::SymbolV(string) => Gc::strong_count(&string.0),
        SteelVal::Custom(gc) => Gc::strong_count(gc),
        SteelVal::HashMapV(map) => Gc::strong_count(&map.0),
        SteelVal::HashSetV(set) => Gc::strong_count(&set.0),
        SteelVal::CustomStruct(s) => s.strong_count(),
        SteelVal::PortV(port) => Gc::strong_count(&port.port),
        SteelVal::IterV(gc) => Gc::strong_count(gc),
        SteelVal::ReducerV(gc) => Gc::strong_count(gc),
        SteelVal::FutureV(gc) => Gc::strong_count(gc),
        SteelVal::BoxedFunction(gc) => Gc::strong_count(gc),
        SteelVal::ContinuationFunction(continuation) => {
            crate::gc::shared::StandardShared::strong_count(&continuation.inner)
        }
        SteelVal::ListV(list) => list.strong_count(),
        SteelVal::Pair(gc) => Gc::strong_count(gc),
        SteelVal::BoxedIterator(gc) => Gc::strong_count(gc),
        SteelVal::SyntaxObject(gc) => Gc::strong_count(gc),
        SteelVal::Boxed(gc) => Gc::strong_count(gc),
        SteelVal::Reference(gc) => Gc::strong_count(gc),
        SteelVal::BigNum(gc) => Gc::strong_count(gc),
        SteelVal::BigRational(gc) => Gc::strong_count(gc),
        SteelVal::Complex(gc) => Gc::strong_count(gc),
        SteelVal::ByteVector(bytes) => Gc::strong_count(&bytes.vec),
        SteelVal::FlatVector(gc) => Gc::strong_count(gc),
        _ => return None,
    })
}

/// Whether `value` is of a kind that holds nothing, which the marker skips
/// before `Watched` gets a look at it.
#[cfg(feature = "sync")]
fn unwatched(value: &SteelVal) -> bool {
    matches!(
        value,
        SteelVal::StringV(_)
            | SteelVal::SymbolV(_)
            | SteelVal::FutureV(_)
            | SteelVal::BoxedFunction(_)
            | SteelVal::ByteVector(_)
            | SteelVal::BigNum(_)
    )
}

/// Where a box, a mutable vector or a mutable struct lives.
#[cfg(feature = "sync")]
fn cell_address(value: &SteelVal) -> Option<usize> {
    match value {
        SteelVal::HeapAllocated(heap_ref) => Some(heap_ref.as_ptr_usize()),
        SteelVal::MutableVector(heap_ref) => Some(heap_ref.as_ptr_usize()),
        SteelVal::MutableStruct(s) => Some(s.as_ptr() as usize),
        _ => None,
    }
}

// Something `UnreachedWalk` came across.
#[cfg(feature = "sync")]
struct WalkNode {
    address: usize,
    // How many references it had when it was found, less the ones the walk
    // was holding. None for a box, a mutable vector or a mutable struct.
    references: Option<usize>,
    // How many of those the walk has found, in what it went through.
    found: usize,
    // Where what it refers to starts in `UnreachedWalk::edges`. It ends
    // where the next node walked starts.
    edges: (u32, u32),
}

/// A walk out from the waiting values the mark didn't reach, through
/// everything they refer to, stopping at the boxes, mutable vectors and
/// mutable structs the mark did reach.
///
/// It counts the references it finds to each reference counted value on the
/// way. A value with more references than were found, registrations and
/// weak boxes aside, is held by something the walk didn't go through. That
/// is either something in use that the mark doesn't trace, or garbage the
/// walk didn't start from, and the two can't be told apart, so the value
/// and what it refers to are taken to be in use.
///
/// The boxes, mutable vectors and mutable structs it goes through are
/// marked, which keeps what a waiting value holds from being swept.
#[cfg(feature = "sync")]
#[derive(Default)]
struct UnreachedWalk {
    queue: Vec<(SteelVal, u32)>,
    nodes: Vec<WalkNode>,
    by_address: rustc_hash::FxHashMap<usize, u32>,
    edges: Vec<u32>,
    // The node being walked.
    current: Option<u32>,
    // Whether the node being walked is a list, an immutable vector, a hash
    // map or a hash set. Those share their insides with others of their
    // kind, so a reference found in one may be a live one's too. The mark
    // would have said so, except for the kinds in `unwatched`, and a
    // reference to one of those found here is not counted.
    shares: bool,
    // How many of the elements in each list buffer have been gone through.
    // Lists share their buffers, and an element is referred to once by the
    // buffer however many lists show it.
    buffers: rustc_hash::FxHashMap<usize, usize>,
    // Where the entries of the hash maps, hash sets and immutable vectors gone
    // through are stored. Ones made from each other share their storage, and
    // an entry there is one reference however many of them show it.
    slots: rustc_hash::FxHashSet<usize>,
    // Values the walk doesn't look inside, for the marker to go through.
    opaque: Vec<SteelVal>,
    cells_reached: usize,
    vectors_reached: usize,
}

#[cfg(feature = "sync")]
impl UnreachedWalk {
    /// The node for `value`, added and queued if this is the first time it
    /// has been seen. None for a value that isn't held by reference.
    fn node(&mut self, value: SteelVal) -> Option<u32> {
        let address = counted_address(&value).or_else(|| cell_address(&value))?;
        if let Some(node) = self.by_address.get(&address) {
            return Some(*node);
        }

        // One of the references is `value` itself.
        let references = reference_count(&value).map(|count| count.saturating_sub(1));
        let node = self.add(address, references);
        self.queue.push((value, node));
        Some(node)
    }

    fn add(&mut self, address: usize, references: Option<usize>) -> u32 {
        let node = self.nodes.len() as u32;
        self.nodes.push(WalkNode {
            address,
            references,
            found: 0,
            edges: (0, 0),
        });
        self.by_address.insert(address, node);
        node
    }

    // Records that the node being walked refers to `node`.
    fn refer(&mut self, node: u32) {
        self.nodes[node as usize].found += 1;
        self.edges.push(node);
    }

    // Goes on to a value stored at `slot`, counting the reference the first
    // time the slot comes up.
    fn push_slot(&mut self, slot: &SteelVal) {
        if self.slots.insert(slot as *const SteelVal as usize) {
            self.push_back(slot.clone());
        } else if let Some(node) = self.node(slot.clone()) {
            self.edges.push(node);
        }
    }

    // What is found from here on is referred to by `node`.
    fn start(&mut self, node: u32) {
        self.close();
        self.nodes[node as usize].edges.0 = self.edges.len() as u32;
        self.current = Some(node);
        self.shares = false;
    }

    fn close(&mut self) {
        if let Some(current) = self.current.take() {
            self.nodes[current as usize].edges.1 = self.edges.len() as u32;
        }
    }

    /// Which nodes are held by something the walk didn't go through, or are
    /// referred to by one that is. `registered` is how many registrations
    /// hold each value.
    fn held_from_outside(&self, registered: &rustc_hash::FxHashMap<usize, usize>) -> Vec<bool> {
        let weakly_boxed = WEAKLY_BOXED.lock().unwrap_or_else(|e| e.into_inner());
        let mut held = vec![false; self.nodes.len()];
        let mut stack = Vec::new();

        for (index, node) in self.nodes.iter().enumerate() {
            let Some(references) = node.references else {
                continue;
            };
            let weak = registered.get(&node.address).copied().unwrap_or(0)
                + weakly_boxed
                    .get(&node.address)
                    .map_or(0, |boxes| boxes.load(Ordering::SeqCst));
            if references > node.found + weak {
                held[index] = true;
                stack.push(index);
            }
        }
        drop(weakly_boxed);

        while let Some(index) = stack.pop() {
            let (from, to) = self.nodes[index].edges;
            for referred in &self.edges[from as usize..to as usize] {
                let referred = *referred as usize;
                if !held[referred] {
                    held[referred] = true;
                    stack.push(referred);
                }
            }
        }

        held
    }
}

// Goes through what the marker goes through, apart from custom types, which
// are left to it.
#[cfg(feature = "sync")]
impl BreadthFirstSearchSteelValVisitor for UnreachedWalk {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    fn pop_front(&mut self) -> Option<SteelVal> {
        let (value, node) = self.queue.pop()?;
        self.start(node);
        Some(value)
    }

    fn push_back(&mut self, value: SteelVal) {
        let uncounted = self.shares && unwatched(&value);
        if let Some(node) = self.node(value) {
            if uncounted {
                self.edges.push(node);
            } else {
                self.refer(node);
            }
        }
    }

    fn visit_closure(&mut self, closure: Gc<ByteCodeLambda>) -> Self::Output {
        for capture in closure.captures() {
            self.push_back(capture.clone());
        }

        if let Some(contract) = closure.get_contract_information().as_ref() {
            self.push_back(contract.clone());
        }
    }

    fn visit_continuation(&mut self, continuation: Continuation) -> Self::Output {
        // Read in place. A clone would add a reference to everything in it.
        let continuation = continuation.inner.read();

        // The values of one that is still open are on a stack.
        if let ContinuationMark::Closed(continuation) = &*continuation {
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

                if let Some(handler) = frame.attachments.as_ref().and_then(|x| x.handler.clone()) {
                    self.push_back(handler);
                }
            }
        }
    }

    // What one of these holds is its own business. Nothing in it is counted,
    // so a value held by one is taken to be in use.
    fn visit_custom_type(&mut self, custom_type: GcMut<Box<dyn CustomType>>) -> Self::Output {
        self.opaque.push(SteelVal::Custom(custom_type));
    }

    fn visit_hash_map(&mut self, hashmap: SteelHashMap) -> Self::Output {
        self.shares = true;
        for (key, value) in hashmap.iter() {
            self.push_slot(key);
            self.push_slot(value);
        }
    }

    fn visit_hash_set(&mut self, hashset: SteelHashSet) -> Self::Output {
        self.shares = true;
        for value in hashset.iter() {
            self.push_slot(value);
        }
    }

    // A cell the mark reached is held by something live, and everything it
    // holds has been marked. The walk doesn't go through it.
    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        let Some(cell) = heap_ref.inner.upgrade() else {
            return;
        };
        if unsafe { cell.get_value().mark_reachable_was_set() } {
            return;
        }

        self.cells_reached += 1;
        let value = cell.lock().value.clone();
        self.push_back(value);
    }

    fn visit_mutable_vector(&mut self, vector: HeapRef<HeapVec>) -> Self::Output {
        let Some(cell) = vector.inner.upgrade() else {
            return;
        };
        if unsafe { cell.get_value().mark_reachable_was_set() } {
            return;
        }

        self.vectors_reached += 1;
        // One at a time. A copy of an element still waiting would count as
        // an outside reference when the same value comes up earlier in the
        // vector.
        let vector = cell.lock();
        for value in vector.value.iter() {
            self.push_back(value.clone());
        }
    }

    fn visit_mutable_struct(&mut self, steel_struct: MutableStructRef) -> Self::Output {
        let cell = steel_struct.cell();
        if cell.mark_reachable_was_set() {
            return;
        }

        self.cells_reached += 1;
        for field in unsafe { cell.fields_unlocked() } {
            self.push_back(field.clone());
        }
    }

    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        self.shares = true;
        for value in vector.iter() {
            self.push_slot(value);
        }
    }

    // A list is a chain of cells, each showing some of a buffer. Lists share
    // their tails, so each cell is a node of its own, referred to by the
    // lists that start at it and by the cell before it.
    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        let mut cell = list.as_ptr();

        loop {
            self.shares = true;
            let next = unsafe {
                List::<SteelVal>::call_from_raw(cell, |cell| {
                    let (buffer, shown) = cell.identity_tuple();
                    let seen = self.buffers.entry(buffer).or_insert(0);
                    let unseen = shown.saturating_sub(*seen);
                    *seen = (*seen).max(shown);

                    // The newest come first, and those are the ones a list
                    // showing fewer hasn't been through.
                    for value in cell.current_node_iter().take(unseen) {
                        self.push_back(value.clone());
                    }

                    cell.next_cell()
                })
            };

            let Some(next) = next else {
                break;
            };
            let (address, references) = unsafe {
                List::<SteelVal>::call_from_raw(next, |next| {
                    (next.as_ptr_usize(), next.strong_count())
                })
            };

            if let Some(node) = self.by_address.get(&address).copied() {
                self.refer(node);
                break;
            }

            let node = self.add(address, Some(references));
            self.refer(node);
            self.start(node);
            cell = next;
        }
    }

    fn visit_pair(&mut self, pair: Gc<super::lists::Pair>) -> Self::Output {
        self.push_back(pair.car());
        self.push_back(pair.cdr());
    }

    fn visit_steel_struct(&mut self, steel_struct: StructRef) -> Self::Output {
        for field in steel_struct.fields().iter() {
            self.push_back(field.clone());
        }
    }

    fn visit_boxed_iterator(&mut self, iterator: GcMut<OpaqueIterator>) -> Self::Output {
        self.push_back(iterator.read().root.clone());
    }

    fn visit_boxed_value(&mut self, boxed_value: GcMut<SteelVal>) -> Self::Output {
        self.push_back(boxed_value.read().clone());
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

    fn visit_bytevector(&mut self, _bytevector: crate::rvals::SteelByteVector) -> Self::Output {}
    fn visit_bignum(&mut self, _: Gc<BigInt>) -> Self::Output {}
    fn visit_complex(&mut self, _: Gc<SteelComplex>) -> Self::Output {}
    fn visit_bool(&mut self, _boolean: bool) -> Self::Output {}
    fn visit_boxed_function(&mut self, _function: Gc<BoxedDynFunction>) -> Self::Output {}
    fn visit_builtin_function(&mut self, _function: BuiltInSignature) -> Self::Output {}
    fn visit_char(&mut self, _c: char) -> Self::Output {}
    fn visit_float(&mut self, _float: f64) -> Self::Output {}
    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) -> Self::Output {}
    fn visit_future(&mut self, _future: Gc<FutureResult>) -> Self::Output {}
    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) -> Self::Output {}
    fn visit_int(&mut self, _int: isize) -> Self::Output {}
    fn visit_rational(&mut self, _: Rational32) -> Self::Output {}
    fn visit_bigrational(&mut self, _: Gc<BigRational>) -> Self::Output {}
    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) -> Self::Output {}
    fn visit_port(&mut self, _port: SteelPort) -> Self::Output {}
    fn visit_reference_value(&mut self, _reference: Gc<OpaqueReference<'static>>) -> Self::Output {}
    fn visit_string(&mut self, _string: SteelString) -> Self::Output {}
    fn visit_symbol(&mut self, _symbol: SteelString) -> Self::Output {}
    fn visit_void(&mut self) -> Self::Output {}
}

impl Custom for WillExecutor {
    fn into_serializable_steelval(&mut self) -> Option<crate::rvals::SerializableSteelVal> {
        Some(crate::rvals::SerializableSteelVal::Void)
    }
}

#[cfg(feature = "sync")]
#[steel_derive::context(name = "make-will-executor", arity = Exact(0))]
pub fn make_will_executor(
    ctx: &mut VmCore,
    _args: &[SteelVal],
) -> Option<Result<SteelVal, SteelErr>> {
    let heap = ctx.thread.heap.data_ptr() as usize;
    Some(WillExecutor::new(heap).into_steelval())
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

    // Wills only become ready in a collection. With none ready, run one
    // before waiting, or a program that never collects waits forever. What
    // one collection frees can be what held a value, so this goes on for as
    // long as a collection frees something.
    let mut collect = true;

    // The registration is only taken once this thread is running again. A
    // collection would not find it in a local.
    let Registration { value, finalizer } = loop {
        if let Some(registration) = executor.take_ready() {
            break registration;
        }

        if collect {
            collect = ctx.gc_collect() > 0;
            continue;
        }

        ctx.thread.enter_safepoint(|_| executor.wait_until_ready());
    };

    apply(ctx, &[finalizer, SteelVal::ListV(vec![value].into())])
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
    value: Arc<SpinLock<Option<WeaklyHeld>>>,
}

// Every weak box that may still hold something, for a collection to go
// through.
#[cfg(feature = "sync")]
static WEAK_BOXES: Mutex<Vec<std::sync::Weak<SpinLock<Option<WeaklyHeld>>>>> =
    Mutex::new(Vec::new());

// Empties the weak boxes whose value nothing else holds. Until a box is
// emptied its value still holds whatever it refers to, which would keep a
// will on one of those from ever being ready.
#[cfg(feature = "sync")]
fn clear_dead_weak_boxes() {
    let mut dead = Vec::new();
    WEAK_BOXES
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .retain(|slot| {
            let Some(slot) = slot.upgrade() else {
                return false;
            };
            let mut held = slot.lock();
            match held.as_ref() {
                Some(value) if value.is_alive() => true,
                _ => {
                    dead.extend(held.take());
                    false
                }
            }
        });
    // Outside the lock, since this drops the values.
    drop(dead);
}

#[cfg(not(feature = "sync"))]
fn clear_dead_weak_boxes() {}

impl Custom for WeakBox {}

// How many weak boxes hold each reference counted value, by its address. A
// weak box has a reference of its own, which is not one that keeps the value
// alive or keeps a will from being ready.
#[cfg(feature = "sync")]
static WEAKLY_BOXED: Lazy<Mutex<rustc_hash::FxHashMap<usize, Arc<AtomicUsize>>>> =
    Lazy::new(|| Mutex::new(rustc_hash::FxHashMap::default()));

// What a weak box holds.
struct WeaklyHeld {
    value: SteelVal,
    // The count of the weak boxes holding the value, this one included.
    // None for a value that isn't reference counted.
    #[cfg(feature = "sync")]
    boxes: Option<Arc<AtomicUsize>>,
}

impl WeaklyHeld {
    fn new(value: SteelVal) -> Self {
        #[cfg(feature = "sync")]
        let boxes = counted_address(&value).map(|address| {
            let mut boxed = WEAKLY_BOXED.lock().unwrap_or_else(|e| e.into_inner());
            let boxes = boxed.entry(address).or_default().clone();
            boxes.fetch_add(1, Ordering::SeqCst);
            boxes
        });

        Self {
            value,
            #[cfg(feature = "sync")]
            boxes,
        }
    }

    // How many weak boxes hold the value.
    fn boxes(&self) -> usize {
        #[cfg(feature = "sync")]
        {
            self.boxes
                .as_ref()
                .map_or(1, |boxes| boxes.load(Ordering::SeqCst))
        }

        #[cfg(not(feature = "sync"))]
        {
            1
        }
    }

    // Whether anything but weak boxes holds the value.
    fn is_alive(&self) -> bool {
        match &self.value {
            // A box or mutable vector is alive if the last mark reached it
            // or it was allocated since.
            SteelVal::HeapAllocated(heap_ref) => heap_ref.is_reachable(),
            SteelVal::MutableVector(heap_ref) => heap_ref.is_reachable(),
            // A struct that a mark didn't reach has been emptied.
            SteelVal::MutableStruct(s) => s.is_reachable() && s.strong_count() != 1,
            value => reference_count(value).map_or(true, |count| count > self.boxes()),
        }
    }
}

// The count goes down before the box's own reference does, so nobody ever
// sees fewer references than there are weak boxes.
#[cfg(feature = "sync")]
impl Drop for WeaklyHeld {
    fn drop(&mut self) {
        let Some(boxes) = self.boxes.take() else {
            return;
        };
        let mut boxed = WEAKLY_BOXED.lock().unwrap_or_else(|e| e.into_inner());
        if boxes.fetch_sub(1, Ordering::SeqCst) == 1 {
            if let Some(address) = counted_address(&self.value) {
                boxed.remove(&address);
            }
        }
    }
}

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
    let value = Arc::new(SpinLock::new(Some(WeaklyHeld::new(args[0].clone()))));
    #[cfg(feature = "sync")]
    WEAK_BOXES
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .push(Arc::downgrade(&value));
    Some(WeakBox { value }.into_steelval())
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
    // Not while a collection is under way, which resets what it has reached.
    leave_safepoint_for_cells();

    let inner = WeakBox::as_ref(value)?;
    let mut guard = inner.value.lock();

    if let Some(held) = guard.as_ref().filter(|held| held.is_alive()) {
        return Ok(held.value.clone());
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
fn next_free_list_id() -> u32 {
    static NEXT: core::sync::atomic::AtomicU32 = core::sync::atomic::AtomicU32::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// The handle of a box that a thread dropped the last handle to, kept so the
/// box can be handed out again.
///
/// A freed slot otherwise waits for the allocation cursor to come around, by
/// which time it has left the cache. The box a thread just emptied is the one
/// it can write most cheaply. While the handle is kept the heap counts the
/// slot as in use, so reusing it needs no bookkeeping, and the handle itself
/// becomes the new box's.
pub struct SpareHandle<T: HeapAble> {
    owner: u32,
    handle: steel_rc::weak::Weak<SpinLock<HeapAllocated<T>>>,
}

// How many a thread keeps.
fn spare_handles() -> usize {
    crate::config::config().gc_spare_handles
}

thread_local! {
    static SPARE_BOXES: RefCell<Vec<SpareHandle<SteelVal>>> = const { RefCell::new(Vec::new()) };
}

#[derive(Debug)]
struct FreeList<T: HeapAble> {
    // TODO: Make this a linked list of vectors. Can reuse im-lists?
    // Pointer chasing may not be worth it. The issue is every doubling of size _also_
    // allocate a whole bunch more. Can probably just get away with a vec of vecs to avoid
    // copying everything.
    elements: Vec<HeapElement<T>>,
    cursor: usize,

    // Which list this is, stamped on its cells.
    id: u32,

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
        let id = next_free_list_id();
        Self {
            cursor: self.cursor,
            id,
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
                        owner: id,
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
                    watched: None,
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
            id: next_free_list_id(),
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
        let id = self.id;
        self.elements.extend(
            core::iter::repeat_with(|| {
                steel_rc::weak::Arc::new(SpinLock::new(HeapAllocated::owned_by(id, T::empty())))
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

    /// Gives up the handles this thread kept, so a collection sees their
    /// slots as free. Counted as in use, they would make the heap look fuller
    /// than it is and grow it.
    fn return_spares() {
        T::with_spare_handles(|spares| spares.clear());
    }

    /// A box of this list that the thread emptied and kept the handle of.
    #[inline]
    fn take_spare(&self) -> Option<steel_rc::weak::Weak<SpinLock<HeapAllocated<T>>>> {
        let id = self.id;
        T::with_spare_handles(|spares| {
            while let Some(spare) = spares.pop() {
                // Another list's box is no use here. One that a collection
                // released since has no heap behind it, and reads a count of
                // zero.
                if spare.owner == id && spare.handle.weak_count() == 1 {
                    return Some(spare.handle);
                }
            }
            None
        })
        .flatten()
    }

    #[inline]
    fn allocate(&mut self, value: T) -> HeapRef<T> {
        if let Some(handle) = self.take_spare() {
            // Nothing else has a handle to it, and the collector only runs
            // under the heap lock this caller holds.
            let cell = unsafe { (*handle.as_ptr()).get_value_mut() };
            // It was emptied when it was kept.
            unsafe { core::ptr::write(&mut cell.value, value) };
            // A collection since then would have cleared this.
            cell.mark_reachable();
            return HeapRef {
                inner: core::mem::ManuallyDrop::new(handle),
            };
        }

        // Drain, moving values around...
        // is that expensive?
        let guard = &mut self.elements[self.cursor];

        // A free slot has no handles, so nothing else can reach it, and the
        // collector only runs under the heap lock this caller holds. Its own
        // lock would only add two atomic updates to every allocation.
        let cell = unsafe { guard.get_value_mut() };

        // TODO: If the guard is registered with a will executor,
        // then we shouldn't mark this value as eligible to be
        // freed?
        //
        // A free slot holds the empty value, which has nothing to drop.
        unsafe { core::ptr::write(&mut cell.value, value) };

        cell.mark_reachable();
        // For the same reason the handle count can't change under this.
        let weak_ptr = unsafe { steel_rc::weak::Arc::downgrade_unshared(guard) };

        // self.elements[self.cursor] = pointer;
        self.alloc_count -= 1;

        // Find where to assign the next slot optimistically?
        let next_slot = self.elements[self.cursor..]
            .iter()
            .position(|x| slot_is_free(x));

        if let Some(next_slot) = next_slot {
            self.cursor += next_slot;
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

        HeapRef {
            inner: core::mem::ManuallyDrop::new(weak_ptr),
        }
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
                // The only caller passes "has no handles", so nothing else can
                // reach this slot, and the heap lock is held. The slot's own
                // lock would only add two atomic updates per dead slot.
                let dropped = {
                    let cell = unsafe { x.get_value_mut() };

                    if !cell.is_reachable() {
                        return;
                    }

                    cell.reset();
                    amount_dropped += 1;
                    core::mem::replace(&mut cell.value, T::empty())
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
        Self::return_spares();
        // Just mark them to be dead
        let res = self.collect_on_condition(|inner| steel_rc::weak::Arc::weak_count(inner) == 0);
        #[cfg(debug_assertions)]
        {
            assert!(!self.elements[self.cursor].lock().is_reachable());
        }
        res
    }

    fn mark_all_unreachable(&mut self) {
        Self::return_spares();
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
            id: next_free_list_id(),
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

        HeapRef {
            inner: core::mem::ManuallyDrop::new(weak_ptr),
        }
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

        HeapRef {
            inner: core::mem::ManuallyDrop::new(weak_ptr),
        }
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
/// The handles of emptied mutable structs a thread keeps, by field count.
/// The mutable struct counterpart of `SpareHandle`: the struct a thread just
/// emptied is the cheapest one for it to fill again. A spare has no handles,
/// so the entry is the cell itself, and its state says a thread holds it.
struct SpareStructs {
    by_len: [Vec<MutableStructCell>; SPARE_STRUCT_LENGTHS],
}

// A thread that exits hands its spares back, or they are never freed.
impl Drop for SpareStructs {
    fn drop(&mut self) {
        for stack in self.by_len.iter_mut() {
            for cell in stack.drain(..) {
                release_spare_struct(cell);
            }
        }
    }
}

// Structs with more fields than this go back to the heap.
const SPARE_STRUCT_LENGTHS: usize = 9;

thread_local! {
    static SPARE_STRUCTS: RefCell<SpareStructs> = const {
        RefCell::new(SpareStructs {
            by_len: [const { Vec::new() }; SPARE_STRUCT_LENGTHS],
        })
    };
}

/// Called when the last handle to a mutable struct went away and it has
/// been emptied. Keeps the cell for this thread's next allocation of the same
/// size, or leaves it for the heap to free.
///
/// Returns false if the heap is gone and the caller has to free the cell.
pub(crate) fn retire_mutable_struct(cell: MutableStructCell) -> bool {
    let len = cell.len();
    let spare = len < SPARE_STRUCT_LENGTHS
        && SPARE_STRUCTS
            .try_with(|spares| {
                let Ok(mut spares) = spares.try_borrow_mut() else {
                    return false;
                };
                let stack = &mut spares.by_len[len];
                if stack.len() >= spare_handles() {
                    return false;
                }
                if !cell.move_state(MUTABLE_STRUCT_LIVE, MUTABLE_STRUCT_SPARE) {
                    return false;
                }
                stack.push(cell);
                true
            })
            .unwrap_or(false);

    // Nothing may touch the cell after it is dead, since the heap can free it.
    spare || cell.move_state(MUTABLE_STRUCT_LIVE, MUTABLE_STRUCT_DEAD)
}

// Set for good once a second thread is started.
//
// A box, a mutable vector and a mutable struct each have a lock around their
// contents, and a thread that is alone skips it. Reading a slot is a load and
// then a count increment on what was loaded, and the slot's own reference is
// all that keeps that alive in between, so a second thread storing into the
// slot at that point frees it under the reader. Nothing a thread checks on
// the cell can rule that out, since the other thread can arrive after the
// check. So the lock is only skipped while there is nobody to arrive.
//
// The flag is set by the thread that starts the second one, before it starts.
// That thread is in the call that does so, which is not in the middle of
// using a cell.
static CELLS_SHARED: AtomicBool = AtomicBool::new(false);

/// Whether more than one thread has ever run, so that cells have to be locked.
#[inline(always)]
pub(crate) fn cells_shared() -> bool {
    CELLS_SHARED.load(Ordering::Relaxed)
}

/// Called before reading or writing a shared cell. A thread in a primitive
/// is at a safepoint, but a value it takes out of a cell is on no stack
/// until the primitive returns, so a collection would miss it. This leaves
/// the safepoint for the rest of the call.
#[inline]
pub(crate) fn leave_safepoint_for_cells() {
    if cells_shared() {
        leave_published_safepoint();
    }
}

#[inline(never)]
fn leave_published_safepoint() {
    let thread = MY_RUNNING
        .try_with(|current| {
            let current = current.try_borrow().ok()?;
            let (_, state) = current.as_ref()?;
            Some(state.thread.load(Ordering::SeqCst))
        })
        .ok()
        .flatten();

    if let Some(thread) = thread.filter(|thread| !thread.is_null()) {
        unsafe { (*thread).leave_safepoint_early() };
    }
}

/// Called before another thread is started.
pub(crate) fn share_cells() {
    CELLS_SHARED.store(true, Ordering::SeqCst);
}

/// Where `cells_shared` reads from, for code that checks it inline.
pub(crate) fn cells_shared_address() -> usize {
    &CELLS_SHARED as *const AtomicBool as usize
}

// A thread that runs steel code: the slot it publishes its context in while
// it is at a safepoint, and how many runs of the interpreter it is inside.
// An engine that isn't running anything has an empty slot too, which is why
// the count is kept.
struct Mutator {
    slot: std::sync::Weak<core::sync::atomic::AtomicPtr<SteelThread>>,
    state: Arc<MutatorState>,
}

#[derive(Default)]
pub(crate) struct MutatorState {
    running: core::sync::atomic::AtomicUsize,
    // How many times the thread has checked in. See `request_check_ins`.
    check_ins: core::sync::atomic::AtomicUsize,
    // The thread's state while it is inside a run, for `send_to_safepoint`.
    thread: core::sync::atomic::AtomicPtr<SteelThread>,
    // Held to write through `thread`, and to take it away.
    thread_in_use: Mutex<()>,
    // The heap the thread allocates from, by its address. Threads read
    // each other's cells only if they share one, so those are the only
    // threads that wait on each other. Two engines in one process don't.
    family: core::sync::atomic::AtomicUsize,
}

// Whether `mutator` shares a heap with the thread asking, and so could be
// reading one of its cells. With no run under way to say which heap that
// is, every thread is taken to.
fn in_my_family(mutator: &Mutator, mine: Option<usize>) -> bool {
    match mine {
        Some(mine) => mutator.state.family.load(Ordering::SeqCst) == mine,
        None => true,
    }
}

fn my_family() -> Option<usize> {
    MY_RUNNING
        .try_with(|current| {
            let current = current.try_borrow().ok()?;
            let (_, state) = current.as_ref()?;
            Some(state.family.load(Ordering::SeqCst))
        })
        .ok()
        .flatten()
}

impl MutatorState {
    /// Gets the thread to its next safepoint soon, if it is inside a run.
    /// Whoever calls this has already left something for it to find there.
    pub(crate) fn send_to_safepoint(&self) {
        let _held = self.thread_in_use.lock();
        let thread = self.thread.load(Ordering::SeqCst);
        if thread.is_null() {
            return;
        }
        // The thread reads this word without synchronization, which is
        // fine for a word.
        #[cfg(feature = "jit2")]
        unsafe {
            let limit = core::ptr::addr_of_mut!((*thread).native_stack_limit);
            core::sync::atomic::AtomicUsize::from_ptr(limit)
                .store(CHECK_IN_STACK_LIMIT, Ordering::SeqCst);
        }
        // Without the jit nothing reads a cell without its lock
        #[cfg(not(feature = "jit2"))]
        let _ = thread;
    }
}

// What a thread's native stack limit is set to when another thread is waiting
// on it to check in.
//
// Jitted code only reaches a safepoint when it calls out, so a thread in a
// loop or a recursion that calls nothing is never seen at one, and what other
// threads have put aside to drop would wait on it for as long as it runs. Its
// calls already compare the stack pointer with this limit, and with the limit
// here they find no room and continue in the interpreter, which checks in at
// its next poll and puts the limit back. A jitted loop compares the limit
// with this value each time round. So asking costs a thread nothing until it
// is asked.
//
// A check in is a point where the thread holds no value it read out of a
// cell without a reference, the same as a safepoint, so a thread whose count
// has moved since a value was put aside can't still be reading that value.
pub(crate) const CHECK_IN_STACK_LIMIT: usize = usize::MAX;

/// Asks every other thread that is inside a run to check in. Also how a
/// collection gets a thread out of jitted code, which never looks at the
/// flag that asks it to stop: in the interpreter the thread does.
pub(crate) fn request_check_ins() {
    let mine = MY_SAFEPOINT_SLOT.with(|mine| mine.get());
    let family = my_family();
    let Ok(mutators) = MUTATORS.lock() else {
        return;
    };
    for mutator in mutators.iter() {
        if mutator.slot.as_ptr() as usize == mine || !in_my_family(mutator, family) {
            continue;
        }
        mutator.state.send_to_safepoint();
    }
}

/// Notes that the calling thread is at a point where it holds nothing it
/// read out of a cell without taking a reference.
pub(crate) fn check_in() {
    let turn = MY_RUNNING
        .try_with(|current| {
            let current = current.try_borrow().ok()?;
            let (_, state) = current.as_ref()?;
            Some(state.check_ins.fetch_add(1, Ordering::SeqCst))
        })
        .ok()
        .flatten();

    // The thread that asked may not come back for a while, so the request
    // is closed from here.
    if turn.is_some() {
        resolve_awaited(false);
    }
}

// Values waiting on these threads to each check in once more.
struct AwaitedBatch {
    values: Vec<Deferred>,
    seen: Vec<(Mutator, usize)>,
    // The thread that put the values aside.
    requester: Option<Arc<MutatorState>>,
    // Whether the request for check ins is still open.
    open: bool,
}

static AWAITED: Mutex<Vec<AwaitedBatch>> = Mutex::new(Vec::new());

// Closes the requests whose threads have all checked in, and drops the
// batches among them that this thread put aside.
//
// The batches of other threads are left for those threads. Most of what is
// in one was made by the thread that put it aside, and dropping another
// thread's values only queues them for that thread, which would move the
// backlog instead of freeing it. `any` takes them regardless, for a
// collection, since their thread may be gone.
fn resolve_awaited(any: bool) {
    let me = MY_RUNNING
        .try_with(|current| {
            let current = current.try_borrow().ok()?;
            current.as_ref().map(|(_, state)| state.clone())
        })
        .ok()
        .flatten();

    let ready: Vec<AwaitedBatch> = {
        let Ok(mut awaited) = AWAITED.try_lock() else {
            return;
        };
        let mut ready = Vec::new();
        let mut index = 0;
        while index < awaited.len() {
            let batch = &mut awaited[index];
            if batch.open && batch.seen.iter().all(|(m, seen)| has_passed(m, *seen)) {
                batch.open = false;
            }

            let mine = match (&batch.requester, &me) {
                (Some(requester), Some(me)) => Arc::ptr_eq(requester, me),
                _ => false,
            };
            if !batch.open && (mine || any) {
                ready.push(awaited.swap_remove(index));
            } else {
                index += 1;
            }
        }
        ready
    };

    // Outside the lock, since dropping them can put more aside.
    drop(ready);
}

// How many values a thread puts aside before it asks the others to check in.
const DEFERRED_DROP_BACKLOG: usize = 4096;

static MUTATORS: Mutex<Vec<Mutator>> = Mutex::new(Vec::new());

// Values other threads put aside and exited before they could drop, with
// the family each thread was in. Only that family's threads read them, so
// only a thread of that family knows when nothing is.
static ORPHANED_DROPS: Mutex<Vec<(Option<usize>, Vec<Deferred>)>> = Mutex::new(Vec::new());

// How many values a thread puts aside between attempts to drop them.
const DEFERRED_DROP_BATCH: usize = 64;

// Something a thread took out of a cell that another thread may still be
// reading.
enum Deferred {
    Value(#[allow(dead_code)] SteelVal),
    // The emptied storage of a vector that grew.
    Buffer(#[allow(dead_code)] HeapVec),
}

// Deferred drops by the family the thread was in. A thread can run more
// than one engine, and only threads of a value's family can be reading it.
struct DeferredDrops(Vec<(Option<usize>, Vec<Deferred>)>);

impl DeferredDrops {
    fn of(&mut self, family: Option<usize>) -> &mut Vec<Deferred> {
        let index = match self.0.iter().position(|(theirs, _)| *theirs == family) {
            Some(index) => index,
            None => {
                self.0.push((family, Vec::new()));
                self.0.len() - 1
            }
        };
        &mut self.0[index].1
    }
}

impl Drop for DeferredDrops {
    fn drop(&mut self) {
        let left: Vec<_> = core::mem::take(&mut self.0)
            .into_iter()
            .filter(|(_, values)| !values.is_empty())
            .collect();
        if left.is_empty() {
            return;
        }
        if let Ok(mut orphaned) = ORPHANED_DROPS.lock() {
            orphaned.extend(left);
        } else {
            // Leaking is the only safe thing left to do with them.
            core::mem::forget(left);
        }
    }
}

thread_local! {
    // The slot this thread publishes its own context in.
    static MY_SAFEPOINT_SLOT: Cell<usize> = const { Cell::new(0) };

    // The slot and count of the engine this thread last ran.
    static MY_RUNNING: RefCell<Option<(usize, Arc<MutatorState>)>> = const { RefCell::new(None) };

    static DEFERRED_DROPS: RefCell<DeferredDrops> = const { RefCell::new(DeferredDrops(Vec::new())) };
}

// The running count for a slot, adding the slot if it isn't known yet.
fn running_count(slot: &Arc<core::sync::atomic::AtomicPtr<SteelThread>>) -> Arc<MutatorState> {
    let address = Arc::as_ptr(slot) as usize;
    let mut mutators = MUTATORS.lock().unwrap_or_else(|e| e.into_inner());
    mutators.retain(|mutator| mutator.slot.strong_count() > 0);
    if let Some(mutator) = mutators
        .iter()
        .find(|mutator| mutator.slot.as_ptr() as usize == address)
    {
        return mutator.state.clone();
    }
    let state = Arc::new(MutatorState::default());
    mutators.push(Mutator {
        slot: Arc::downgrade(slot),
        state: state.clone(),
    });
    state
}

/// Records a thread that will run steel code.
pub(crate) fn register_mutator(slot: &Arc<core::sync::atomic::AtomicPtr<SteelThread>>) {
    running_count(slot);
}

/// Held while a thread is inside a run of the interpreter. Runs nest, and a
/// nested one can belong to another engine, so leaving one puts back what
/// the thread was running before.
pub(crate) struct Running {
    state: Arc<MutatorState>,
    outer: Option<(usize, Arc<MutatorState>)>,
    outer_thread: *mut SteelThread,
}

impl Drop for Running {
    fn drop(&mut self) {
        self.state.running.fetch_sub(1, Ordering::SeqCst);
        {
            let _held = self.state.thread_in_use.lock();
            self.state.thread.store(self.outer_thread, Ordering::SeqCst);
        }
        let outer = self.outer.take();
        MY_SAFEPOINT_SLOT.with(|mine| mine.set(outer.as_ref().map(|x| x.0).unwrap_or(0)));
        let _ = MY_RUNNING.try_with(|current| *current.borrow_mut() = outer);
    }
}

impl Running {
    pub(crate) fn state(&self) -> &Arc<MutatorState> {
        &self.state
    }
}

/// Called on the thread that is about to run steel code with this slot.
#[inline]
pub(crate) fn enter_mutator(
    slot: &Arc<core::sync::atomic::AtomicPtr<SteelThread>>,
    thread: *mut SteelThread,
) -> Running {
    let address = Arc::as_ptr(slot) as usize;

    let (running, outer) = MY_RUNNING.with(|current| {
        let mut current = current.borrow_mut();
        let running = match &*current {
            Some((known, running)) if *known == address => running.clone(),
            _ => running_count(slot),
        };
        let outer = current.replace((address, running.clone()));
        (running, outer)
    });
    MY_SAFEPOINT_SLOT.with(|mine| mine.set(address));

    let outer_thread = running.thread.swap(thread, Ordering::SeqCst);
    let family = unsafe { (*thread).heap.data_ptr() as usize };
    running.family.store(family, Ordering::SeqCst);

    // Counted even when alone, since another thread can start mid run
    running.running.fetch_add(1, Ordering::SeqCst);
    // Starting a run is a point with nothing in hand, and a thread that
    // wasn't running when another asked was never told.
    running.check_ins.fetch_add(1, Ordering::SeqCst);

    Running {
        state: running,
        outer,
        outer_thread,
    }
}

// Whether every other thread was seen at a safepoint, not running, or gone.
//
// Jitted code reads a cell without its lock, and between loading a value and
// taking a reference to it nothing but the cell keeps the value alive. That
// stretch has no safepoint in it. So a thread seen at a safepoint after a
// value left its cell is not in the middle of reading that value, and what it
// reads next comes from the cell as it is now. The thread's own store on the
// way out of a safepoint and this load are both sequentially consistent, so
// one of them sees the other.
fn others_at_safepoints() -> bool {
    let mine = MY_SAFEPOINT_SLOT.with(|mine| mine.get());
    let family = my_family();
    let Ok(mutators) = MUTATORS.lock() else {
        return false;
    };
    mutators.iter().all(|mutator| match mutator.slot.upgrade() {
        None => true,
        Some(slot) => {
            Arc::as_ptr(&slot) as usize == mine
                || !in_my_family(mutator, family)
                || mutator.state.running.load(Ordering::SeqCst) == 0
                || !slot.load(Ordering::SeqCst).is_null()
        }
    })
}

// The other threads that are running and not at a safepoint, with their
// check in counts as they are now.
fn threads_to_await() -> Vec<(Mutator, usize)> {
    let mine = MY_SAFEPOINT_SLOT.with(|mine| mine.get());
    let Ok(mutators) = MUTATORS.lock() else {
        return Vec::new();
    };
    let family = my_family();
    mutators
        .iter()
        .filter(|mutator| mutator.slot.as_ptr() as usize != mine)
        .filter(|mutator| in_my_family(mutator, family))
        .map(|mutator| {
            (
                Mutator {
                    slot: mutator.slot.clone(),
                    state: mutator.state.clone(),
                },
                mutator.state.check_ins.load(Ordering::SeqCst),
            )
        })
        .collect()
}

// Whether a thread has been somewhere it holds nothing unprotected since
// its count was `seen`.
fn has_passed(mutator: &Mutator, seen: usize) -> bool {
    match mutator.slot.upgrade() {
        None => true,
        Some(slot) => {
            mutator.state.running.load(Ordering::SeqCst) == 0
                || !slot.load(Ordering::SeqCst).is_null()
                || mutator.state.check_ins.load(Ordering::SeqCst) != seen
        }
    }
}

/// Drops what has been put aside, if no other thread can still be reading
/// it. A backlog the others are keeping alive is handed over to wait on
/// them checking in.
pub(crate) fn drop_deferred() {
    drop_deferred_from(false)
}

/// The same from a collection, which also takes what other threads left.
pub(crate) fn drop_deferred_at_collection() {
    drop_deferred_from(true)
}

fn drop_deferred_from(collection: bool) {
    resolve_awaited(collection);

    // Only look at what was deferred in the current family, since those are
    // the threads we can check. With no family that is everything.
    let family = my_family();

    let ready = DEFERRED_DROPS
        .try_with(|deferred| {
            let Ok(mut deferred) = deferred.try_borrow_mut() else {
                return Vec::new();
            };

            let mut ready = Vec::new();
            let mut at_safepoints = None;

            for (theirs, values) in deferred.0.iter_mut() {
                if values.is_empty() || !(family.is_none() || *theirs == family) {
                    continue;
                }

                if *at_safepoints.get_or_insert_with(others_at_safepoints) {
                    ready.append(values);
                    continue;
                }

                // A collection hands over even a short list. Otherwise it
                // waits for every other thread to be at a safepoint at once,
                // which may never happen in a busy process.
                if collection || values.len() >= DEFERRED_DROP_BACKLOG {
                    let batch = AwaitedBatch {
                        values: core::mem::take(values),
                        seen: threads_to_await(),
                        requester: MY_RUNNING.with(|current| {
                            current.borrow().as_ref().map(|(_, state)| state.clone())
                        }),
                        open: true,
                    };
                    match AWAITED.lock() {
                        Ok(mut awaited) => {
                            awaited.push(batch);
                            drop(awaited);
                            // After the counts were read, so a check in that answers
                            // this is counted.
                            request_check_ins();
                        }
                        Err(_) => *values = batch.values,
                    }
                }
            }

            ready
        })
        .unwrap_or_default();

    if ready.is_empty() {
        return;
    }

    // What this thread just found is that the threads of its own family
    // are reading nothing. With no family, that was every thread.
    let orphaned = ORPHANED_DROPS
        .lock()
        .map(|mut orphaned| {
            let (mine, others) = core::mem::take(&mut *orphaned)
                .into_iter()
                .partition(|(theirs, _)| family.is_none() || *theirs == family);
            *orphaned = others;
            mine
        })
        .unwrap_or_else(|_| Vec::new());

    // Outside the borrow, since dropping them can put more aside.
    drop(ready);
    drop(orphaned);
}

/// Takes a value that was just swapped out of a cell and returns it for the
/// caller to keep.
///
/// With other threads running, one of them may have read the value out of
/// the cell and not yet taken its reference. The cell's own reference is what
/// that reader is relying on, so it is put aside here and dropped once every
/// other thread has been seen at a safepoint, and the caller gets a reference
/// of its own.
#[inline]
pub(crate) fn retire_swapped(old: SteelVal) -> SteelVal {
    if !cells_shared() || !needs_release(&old) {
        return old;
    }

    let kept = old.clone();
    defer(Deferred::Value(old));
    kept
}

fn defer(item: Deferred) {
    let mut item = Some(item);
    let due = DEFERRED_DROPS
        .try_with(|deferred| match deferred.try_borrow_mut() {
            Ok(mut deferred) => {
                let values = deferred.of(my_family());
                values.extend(item.take());
                values.len() % DEFERRED_DROP_BATCH == 0
            }
            Err(_) => false,
        })
        .unwrap_or(false);
    // Only while this thread is tearing down. Leaked instead of dropped
    // under a reader.
    core::mem::forget(item);
    if due {
        drop_deferred();
    }
}

/// Takes a value that was just removed from a cell and is not wanted. See
/// `retire_swapped`.
#[inline]
pub(crate) fn retire_removed(old: SteelVal) {
    if cells_shared() && needs_release(&old) {
        defer(Deferred::Value(old));
    }
}

/// Makes room for `additional` more elements in a vector that other threads
/// read without its lock.
///
/// Such a reader may be about to load an element out of the storage it found
/// in the vector. Growing in place would free that storage under it, so the
/// elements are moved to new storage here and the old is freed once every
/// other thread has been seen at a safepoint. With room already there, or
/// with no other thread, this does nothing and the vector grows by itself.
pub(crate) fn reserve_shared(vector: &mut HeapVec, additional: usize) {
    let len = vector.len();
    let needed = len.saturating_add(additional);
    if !cells_shared() || needed <= vector.capacity() {
        return;
    }

    let capacity = needed.max(vector.capacity().saturating_mul(2)).max(4);
    let mut grown = HeapVec::with_capacity(capacity);
    unsafe {
        core::ptr::copy_nonoverlapping(vector.as_ptr(), grown.as_mut_ptr(), len);
        grown.set_len(len);
        // The elements belong to the new storage now.
        vector.set_len(0);
    }
    defer(Deferred::Buffer(core::mem::replace(vector, grown)));
}

/// Empties a vector that other threads read without its lock.
pub(crate) fn clear_shared(vector: &mut HeapVec) {
    if cells_shared() {
        vector.drain(..).for_each(retire_removed);
    } else {
        vector.clear();
    }
}

// Structs made where the heap wasn't at hand, waiting to join the list of
// the heap they were made for.
static DETACHED_STRUCTS: Mutex<Vec<MutableStructCell>> = Mutex::new(Vec::new());

/// A mutable struct for the heap list `owner`, made without access to that
/// heap. The list picks it up at its next collection. Until then no mark
/// clears its reachable flag, so nothing collects it.
pub(crate) fn detached_mutable_struct(
    descriptor: StructTypeDescriptor,
    owner: u32,
    fields: impl ExactSizeIterator<Item = SteelVal>,
) -> MutableStructRef {
    let (cell, handle) = MutableStructCell::allocate(descriptor, owner, fields);
    DETACHED_STRUCTS.lock().unwrap().push(cell);
    handle
}

// Hands a spare back to whichever heap owns it.
fn release_spare_struct(cell: MutableStructCell) {
    // If the heap is gone the cell is this thread's to free
    if !cell.move_state(MUTABLE_STRUCT_SPARE, MUTABLE_STRUCT_DEAD) {
        unsafe { cell.free() };
    }
}

/// Every mutable struct the heap owns.
///
/// Unlike the boxes of a `FreeList` the cells differ in size, so they are
/// allocated as they are needed and freed once they are dead, and the list
/// only records them for the mark and the sweep.
struct MutableStructList {
    cells: Vec<MutableStructCell>,
    id: u32,
    // Cells added since the list was last scanned for dead ones.
    since_scan: usize,
    scan_budget: usize,
    // The size after the last mark, and how far past it the list may grow
    // before the next.
    floor: usize,
    mark_budget: usize,
    should_run_weak: bool,
}

impl MutableStructList {
    const INITIAL_BUDGET: usize = 256 * 100;

    fn new() -> Self {
        Self {
            cells: Vec::new(),
            id: next_free_list_id(),
            since_scan: 0,
            scan_budget: Self::INITIAL_BUDGET,
            floor: 0,
            mark_budget: Self::INITIAL_BUDGET,
            should_run_weak: true,
        }
    }

    #[inline]
    fn due_for_scan(&self) -> bool {
        self.since_scan >= self.scan_budget
    }

    #[inline]
    fn due_for_mark(&self) -> bool {
        self.cells.len() >= self.floor + self.mark_budget
    }

    #[inline]
    fn take_spare(&self, len: usize) -> Option<MutableStructCell> {
        if len >= SPARE_STRUCT_LENGTHS {
            return None;
        }
        let id = self.id;
        SPARE_STRUCTS
            .try_with(|spares| {
                let mut spares = spares.try_borrow_mut().ok()?;
                let stack = &mut spares.by_len[len];
                while let Some(cell) = stack.pop() {
                    if cell.header().owner == id {
                        return Some(cell);
                    }
                    release_spare_struct(cell);
                }
                None
            })
            .ok()
            .flatten()
    }

    #[inline]
    fn allocate(
        &mut self,
        descriptor: StructTypeDescriptor,
        fields: impl ExactSizeIterator<Item = SteelVal>,
    ) -> MutableStructRef {
        let len = fields.len();
        if let Some(cell) = self.take_spare(len) {
            // The cell has no handles, and the collector only runs under the
            // heap lock this caller holds.
            return unsafe { cell.fill(descriptor, fields) };
        }

        let (cell, handle) = MutableStructCell::allocate(descriptor, self.id, fields);
        self.cells.push(cell);
        self.since_scan += 1;
        handle
    }

    /// Takes in the structs that were made for this list from outside it.
    fn adopt_detached(&mut self) {
        let mut detached = DETACHED_STRUCTS.lock().unwrap();
        if detached.is_empty() {
            return;
        }
        let id = self.id;
        let cells = &mut self.cells;
        detached.retain(|cell| {
            if cell.header().owner == id {
                cells.push(*cell);
                false
            } else {
                true
            }
        });
    }

    /// Gives up the cells this thread kept, so a collection can free them.
    fn return_spares() {
        let _ = SPARE_STRUCTS.try_with(|spares| {
            if let Ok(mut spares) = spares.try_borrow_mut() {
                for stack in spares.by_len.iter_mut() {
                    for cell in stack.drain(..) {
                        release_spare_struct(cell);
                    }
                }
            }
        });
    }

    /// Frees the cells that have no handles left. Returns how many.
    fn free_dead(&mut self) -> usize {
        Self::return_spares();
        self.adopt_detached();

        let before = self.cells.len();
        self.cells.retain(|cell| {
            if cell.state() == MUTABLE_STRUCT_DEAD {
                unsafe { cell.free() };
                false
            } else {
                true
            }
        });
        self.since_scan = 0;
        self.scan_budget = Self::INITIAL_BUDGET.max(self.cells.len() / 2);

        before - self.cells.len()
    }

    fn mark_all_unreachable(&mut self) {
        Self::return_spares();
        self.adopt_detached();
        for cell in &self.cells {
            cell.set_reachable(false);
        }
    }

    /// After a mark, takes the fields of every struct it didn't reach. Those
    /// can still have handles, held by other garbage, and emptying them is
    /// what breaks a cycle. The cell is freed once those handles are gone.
    fn take_unreachable(&self) -> Vec<SteelVal> {
        let mut released = Vec::new();
        for cell in &self.cells {
            if !cell.is_reachable() && cell.state() == MUTABLE_STRUCT_LIVE {
                released.extend(cell.take_fields());
            }
        }
        released
    }

    fn resize_after_collection(&mut self, marking_work: usize) {
        self.floor = self.cells.len();
        self.mark_budget = (marking_work / gc_work_ratio())
            .max(Self::INITIAL_BUDGET)
            .min(gc_max_headroom());
    }
}

// A copied heap starts without any. The values of the copy still point at
// the cells of the original, which stays their owner.
impl Clone for MutableStructList {
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl Drop for MutableStructList {
    fn drop(&mut self) {
        Self::return_spares();
        self.adopt_detached();

        // A handle or another thread's spare stack can outlive the heap.
        // Fields are taken before the cell is orphaned, while only this heap
        // can free it. A dead cell is freed here, any other by its holder.
        let mut released = Vec::new();
        for cell in core::mem::take(&mut self.cells) {
            if cell.state() == MUTABLE_STRUCT_LIVE {
                released.extend(cell.take_fields());
            }
            if cell.orphan() == MUTABLE_STRUCT_DEAD {
                unsafe { cell.free() };
            }
        }
        drop(released);
    }
}

#[derive(Clone)]
pub struct Heap {
    count: usize,
    mark_and_sweep_queue: Vec<SteelVal>,

    maybe_memory_size: usize,

    skip_minor_collection: bool,
    memory_free_list: FreeList<SteelVal>,
    vector_free_list: FreeList<HeapVec>,
    mutable_structs: MutableStructList,
    // How many boxes, vectors and structs the sweeps have emptied.
    swept: usize,
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
            mutable_structs: MutableStructList::new(),
            swept: 0,
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
            mutable_structs: MutableStructList::new(),
            swept: 0,
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

    pub(crate) fn swept(&self) -> usize {
        self.swept
    }

    // With no collection to make room, a full list has to grow, or the
    // allocation would be handed a slot that is in use.
    pub fn allocate_without_collection(&mut self, value: SteelVal) -> HeapRef<SteelVal> {
        if self.memory_free_list.is_heap_full() {
            self.memory_free_list.grow();
        }
        self.memory_free_list.allocate(value)
    }

    pub fn allocate_vec_without_collection(&mut self, value: HeapVec) -> HeapRef<HeapVec> {
        if self.vector_free_list.is_heap_full() {
            self.vector_free_list.grow();
        }
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

    /// A mutable struct holding `values`.
    pub fn allocate_mutable_struct<'a, V>(
        &mut self,
        descriptor: StructTypeDescriptor,
        values: V,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &'a mut Synchronizer,
    ) -> MutableStructRef
    where
        V: AsRef<[SteelVal]> + IntoIterator<Item = SteelVal>,
        V::IntoIter: ExactSizeIterator,
    {
        if self.mutable_structs.due_for_scan() {
            self.mutable_struct_collection(
                values.as_ref().iter().cloned(),
                roots,
                live_functions,
                globals,
                tls,
                synchronizer,
            );
        }

        self.mutable_structs
            .allocate(descriptor, values.into_iter())
    }

    pub fn allocate_mutable_struct_without_collection(
        &mut self,
        descriptor: StructTypeDescriptor,
        values: impl ExactSizeIterator<Item = SteelVal>,
    ) -> MutableStructRef {
        self.mutable_structs.allocate(descriptor, values)
    }

    #[inline(never)]
    fn mutable_struct_collection<'a>(
        &mut self,
        // The fields about to be stored. They are not reachable from any root
        // yet, so a mark here has to be told about them.
        pending: impl Iterator<Item = SteelVal>,
        roots: &'a [SteelVal],
        live_functions: impl Iterator<Item = &'a ByteCodeLambda>,
        globals: &'a [SteelVal],
        tls: &'a [SteelVal],
        synchronizer: &mut Synchronizer,
    ) {
        run_explicit_merge();
        drop_deferred_at_collection();
        clear_dead_weak_boxes();

        // The cheap pass, which frees the structs whose handles are all gone.
        let before = self.mutable_structs.cells.len();
        let reclaimed = self.mutable_structs.free_dead();
        if self.mutable_structs.should_run_weak
            && reclaimed.saturating_mul(weak_yield_ratio()) < before
        {
            self.mutable_structs.should_run_weak = false;
        }

        // What it can't free is live or in a cycle, and only a mark tells
        // those apart.
        if self.mutable_structs.due_for_mark() {
            self.mark_and_sweep_new(
                None,
                pending,
                roots,
                live_functions,
                globals,
                tls,
                synchronizer,
            );

            self.memory_free_list.should_run_weak = true;
            self.vector_free_list.should_run_weak = true;
            self.mutable_structs.should_run_weak = true;
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
            drop_deferred_at_collection();
            clear_dead_weak_boxes();
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
        let fields = self.mutable_structs.take_unreachable();
        self.swept += values.len() + vectors.len() + fields.len();
        drop(values);
        drop(vectors);
        drop(fields);
        self.mutable_structs.free_dead();
        self.mutable_structs
            .resize_after_collection(stats.object_count);
        // Dropping those refilled this thread's spare handles.
        #[cfg(feature = "sync")]
        FreeList::<SteelVal>::return_spares();
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

        #[cfg(feature = "sync")]
        let this_heap = self as *const Heap as usize;

        let mut context = MarkAndSweepContext {
            queue: &mut self.mark_and_sweep_queue,
            stats: MarkAndSweepStats::default(),
            #[cfg(feature = "sync")]
            watched: None,
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
                    if let Some(watched) = &context.watched {
                        watched.note($value);
                    }
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
        synchronizer.wait_for_threads();

        // Only once nothing else is running, since a weak box reads these
        self.memory_free_list.mark_all_unreachable();
        self.vector_free_list.mark_all_unreachable();
        self.mutable_structs.mark_all_unreachable();

        #[cfg(feature = "sync")]
        let executors = will_executors_on(this_heap);
        #[cfg(feature = "sync")]
        let wills = WillCollection::begin(this_heap, &executors);
        #[cfg(feature = "sync")]
        if let Some(wills) = &wills {
            context.watched = wills.watched.clone();
            for value in wills.roots() {
                push_root!(value);
            }
        }

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

            match wills {
                None => count,
                Some(wills) => {
                    let (walked, opaque) = wills.settle();
                    if opaque.is_empty() {
                        count + walked
                    } else {
                        let pointers = opaque
                            .iter()
                            .filter_map(SteelValPointer::from_value)
                            .collect();
                        count + walked + MARKER.mark(&[], pointers)
                    }
                }
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
                        watched: watched(),
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

    /// What `set` returns for a value it swapped out of a cell that other
    /// threads read without its lock. See `retire_swapped`.
    #[inline]
    fn retire(self) -> Self {
        self
    }

    /// Runs `f` on this thread's spare handles for boxes of this type, when it
    /// keeps any. See `SpareHandle`.
    #[inline]
    fn with_spare_handles<R>(_f: impl FnOnce(&mut Vec<SpareHandle<Self>>) -> R) -> Option<R>
    where
        Self: Sized,
    {
        None
    }

    /// Drops a value taken out of a box whose last handle went away, without
    /// recursing through whatever it holds. Otherwise a chain of boxes, such as
    /// a mutable list built from mutable structs, drops one box per native frame.
    fn release(self);
}
impl HeapAble for SteelVal {
    fn empty() -> Self {
        SteelVal::Void
    }

    #[inline]
    fn retire(self) -> Self {
        retire_swapped(self)
    }

    #[inline]
    fn with_spare_handles<R>(f: impl FnOnce(&mut Vec<SpareHandle<Self>>) -> R) -> Option<R> {
        SPARE_BOXES
            .try_with(|spares| {
                spares
                    .try_borrow_mut()
                    .ok()
                    .map(|mut spares| f(&mut spares))
            })
            .ok()
            .flatten()
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
    #[inline]
    fn retire(mut self) -> Self {
        if cells_shared() {
            self.drain(..).for_each(retire_removed);
            defer(Deferred::Buffer(self));
            return HeapVec::new();
        }
        self
    }

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
    // Taken out by `drop` when the handle is kept as a spare.
    pub(crate) inner: core::mem::ManuallyDrop<steel_rc::weak::Weak<SpinLock<HeapAllocated<T>>>>,
}

/// Copies a value out of memory that another thread may be writing. What
/// comes back can be half of one value and half of another, so it must not
/// be used until the caller has checked that no write was in the way.
#[inline(always)]
pub(crate) unsafe fn read_unsteady(slot: *const SteelVal) -> core::mem::ManuallyDrop<SteelVal> {
    const _: () = assert!(core::mem::size_of::<SteelVal>() == 16);

    unsafe { core::mem::transmute(core::ptr::read_volatile(slot as *const [u64; 2])) }
}

impl HeapRef<SteelVal> {
    /// The contents, with a reference taken, while other threads may be
    /// writing the box. The read is checked against the lock's count, and
    /// only takes the lock if a write got in the way. A value that a write
    /// swaps out meanwhile is kept alive until this thread is next at a
    /// safepoint, which is what makes taking the reference safe. Must not be
    /// called from a primitive, which runs at a safepoint.
    #[inline]
    pub(crate) fn read_shared(&self) -> SteelVal {
        let lock = unsafe { &*self.inner.as_ptr() };

        let before = lock.sequence();
        if before & 1 == 0 {
            let value = unsafe { read_unsteady(core::ptr::addr_of!((*lock.data_ptr()).value)) };
            if lock.sequence_after_read() == before {
                return (*value).clone();
            }
        }

        self.read_locked()
    }

    #[cold]
    #[inline(never)]
    fn read_locked(&self) -> SteelVal {
        unsafe { &*self.inner.as_ptr() }.lock().value.clone()
    }
}

impl HeapRef<HeapVec> {
    /// The element at `index`, with a reference taken, while other threads
    /// may be writing the vector. `None` if the index is out of bounds.
    ///
    /// The length and the storage are read as a pair that the lock's count
    /// vouches for before the storage is touched, so the index is in bounds
    /// of that storage. A vector that grows meanwhile keeps its old storage
    /// allocated until this thread is next at a safepoint (see
    /// `reserve_shared`), and so is an element that is swapped out. Must not
    /// be called from a primitive, which runs at a safepoint.
    #[inline]
    pub(crate) fn read_shared(&self, index: usize) -> Option<SteelVal> {
        let lock = unsafe { &*self.inner.as_ptr() };

        let before = lock.sequence();
        if before & 1 == 0 {
            let vector = unsafe { core::ptr::addr_of!((*lock.data_ptr()).value) as *const u8 };
            let (len, buf) = unsafe {
                (
                    core::ptr::read_volatile(vector.add(HeapVec::len_offset()) as *const usize),
                    core::ptr::read_volatile(
                        vector.add(HeapVec::buf_offset()) as *const *const SteelVal
                    ),
                )
            };

            if lock.sequence_after_read() == before {
                if index >= len {
                    return None;
                }

                let value = unsafe { read_unsteady(buf.add(index)) };
                if lock.sequence_after_read() == before {
                    return Some((*value).clone());
                }
            }
        }

        self.read_locked(index)
    }

    #[cold]
    #[inline(never)]
    fn read_locked(&self, index: usize) -> Option<SteelVal> {
        unsafe { &*self.inner.as_ptr() }
            .lock()
            .value
            .get(index)
            .cloned()
    }
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
        let Some(value) = self.take_if_last() else {
            unsafe { core::mem::ManuallyDrop::drop(&mut self.inner) };
            return;
        };

        self.release_last(value);
    }
}

impl<T: HeapAble> HeapRef<T> {
    // What the last handle to a box does with what the box held, and with
    // itself.
    //
    // Kept out of line. Dropping a handle is part of dropping any value, and
    // this, built into that, makes it save more registers on the way in,
    // which every value dropped then pays for, a box or not.
    #[inline(never)]
    fn release_last(&mut self, value: T) {
        value.release();

        // The box is empty and this is its only handle, so keep the handle
        // for the next allocation instead of giving the slot back.
        let handle = unsafe { core::mem::ManuallyDrop::take(&mut self.inner) };
        let cell = unsafe { (*handle.as_ptr()).get_value() };
        if cell.finalizer {
            return;
        }
        let owner = cell.owner;
        T::with_spare_handles(move |spares| {
            if spares.len() < spare_handles() {
                spares.push(SpareHandle { owner, handle });
            }
        });
    }
}

impl<T: HeapAble> HeapRef<T> {
    /// Takes the value out when this is the only handle left, leaving the
    /// box empty.
    #[inline]
    pub(crate) fn take_if_last(&self) -> Option<T> {
        // This also means the heap still holds the cell: `weak_count` reads
        // zero once it has been released.
        if self.inner.weak_count() != 1 {
            return None;
        }
        // So the cell is used in place, without upgrading the handle, which
        // would cost two more atomic updates on every box drop. The heap only
        // releases a cell during a collection, which stops the other threads,
        // so it can't go away between the check above and this. Jitted code
        // reads a box by its address on the same grounds.
        let inner = unsafe { &*self.inner.as_ptr() };
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
        leave_safepoint_for_cells();
        self.inner.upgrade().unwrap().lock().value.clone()
    }

    /// Whether the last mark reached this, or it was allocated since.
    pub(crate) fn is_reachable(&self) -> bool {
        self.inner
            .upgrade()
            .is_some_and(|inner| inner.lock().is_reachable())
    }

    pub unsafe fn inner_ref(&self) -> Option<&T> {
        if self.inner.weak_count() == 1 && !cells_shared() {
            Some(&(&*self.inner.as_ptr()).get_value().value)
        } else {
            None
        }
    }

    pub fn borrow<O>(&self, thunk: impl FnOnce(&T) -> O) -> O {
        leave_safepoint_for_cells();
        let value = self.inner.upgrade().unwrap();
        let value = value.lock();
        thunk(&value.value)
    }

    pub fn as_ptr_usize(&self) -> usize {
        self.inner.as_ptr() as usize
    }

    pub fn set(&mut self, value: T) -> T {
        self.set_and_return(value)
    }

    pub fn set_and_return(&self, value: T) -> T {
        leave_safepoint_for_cells();
        let inner = self.inner.upgrade().unwrap();

        let old = {
            let mut guard = inner.write();
            core::mem::replace(&mut guard.value, value)
        };
        old.retire()
    }

    pub(crate) fn set_interior_mut(&self, value: T) -> T {
        self.set_and_return(value)
    }

    pub(crate) fn strong_ptr(&self) -> steel_rc::weak::Arc<SpinLock<HeapAllocated<T>>> {
        leave_safepoint_for_cells();
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
    // The free list this cell belongs to. See `SpareHandle`.
    pub(crate) owner: u32,
    pub(crate) value: T,
}

impl<T: Clone + core::fmt::Debug + PartialEq + Eq> Clone for HeapAllocated<T> {
    fn clone(&self) -> Self {
        Self {
            reachable: AtomicBool::new(self.is_reachable()),
            finalizer: self.finalizer,
            owner: self.owner,
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
            owner: 0,
            value,
        }
    }

    fn owned_by(owner: u32, value: T) -> Self {
        Self {
            owner,
            ..Self::new(value)
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
    // See `Watched`.
    #[cfg(feature = "sync")]
    watched: Option<Arc<Watched>>,
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
    // See `Watched`.
    watched: Option<Arc<Watched>>,
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

// TODO: @Matt
// Eventually once we decouple the RwLock from the dyn Custom variant, we can get rid of this
//
// This issue is that the GC will block on these values because they're mutably referenced,
// so during the mark phase, the gc visitor hits a mutably referenced value over FFI and blocks,
// but the value _must_ be mutably blocked to avoid double borrows. This ends up in a funny
// situation where any blocking call just stops the GC.
//
// To avoid this, we store a stack of the addresses that we visit and just expose those
// to the gc so that it can skip these. Values passed over FFI are special; they do not
// have the same restrictions since they can't actually hold steel values.
#[cfg(feature = "dylibs")]
pub(crate) mod ffi_borrows {
    use parking_lot::Mutex;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Arc, Weak};

    const CAPACITY: usize = 32;

    struct ThreadBorrows {
        len: AtomicUsize,
        addresses: [AtomicUsize; CAPACITY],
    }

    static THREADS: Mutex<Vec<Weak<ThreadBorrows>>> = Mutex::new(Vec::new());

    thread_local! {
        static BORROWS: Arc<ThreadBorrows> = {
            let borrows = Arc::new(ThreadBorrows {
                len: AtomicUsize::new(0),
                addresses: [const { AtomicUsize::new(0) }; CAPACITY],
            });

            let mut threads = THREADS.lock();
            threads.retain(|thread| thread.strong_count() > 0);
            threads.push(Arc::downgrade(&borrows));

            borrows
        };
    }

    pub(crate) fn push(address: usize) -> bool {
        BORROWS.with(|borrows| {
            let len = borrows.len.load(Ordering::Relaxed);
            if len == CAPACITY {
                return false;
            }

            borrows.addresses[len].store(address, Ordering::Release);
            borrows.len.store(len + 1, Ordering::Release);
            true
        })
    }

    pub(crate) fn pop(count: usize) {
        BORROWS.with(|borrows| {
            let len = borrows.len.load(Ordering::Relaxed);
            borrows.len.store(len - count, Ordering::Release);
        })
    }

    #[cfg(feature = "sync")]
    pub(crate) fn held_by_current_thread(address: usize) -> bool {
        BORROWS.with(|borrows| {
            let len = borrows.len.load(Ordering::Relaxed);
            len == CAPACITY
                || borrows.addresses[..len]
                    .iter()
                    .any(|x| x.load(Ordering::Relaxed) == address)
        })
    }

    #[cfg(feature = "sync")]
    pub(crate) fn contains(address: usize) -> bool {
        THREADS.lock().iter().any(|thread| {
            let Some(thread) = thread.upgrade() else {
                return false;
            };

            let len = thread.len.load(Ordering::Acquire);
            thread.addresses[..len]
                .iter()
                .any(|x| x.load(Ordering::Acquire) == address)
        })
    }
}

#[cfg(all(feature = "sync", feature = "dylibs"))]
fn visit_custom_children(
    custom_type: &MutContainer<Box<dyn CustomType>>,
    context: &mut MarkAndSweepContext,
) {
    let address = custom_type as *const _ as usize;

    loop {
        if let Some(guard) = custom_type.try_read() {
            guard.visit_children(context);
            return;
        }

        if ffi_borrows::contains(address) {
            return;
        }

        std::thread::yield_now();
    }
}

#[cfg(not(all(feature = "sync", feature = "dylibs")))]
fn visit_custom_children(
    custom_type: &MutContainer<Box<dyn CustomType>>,
    context: &mut MarkAndSweepContext,
) {
    #[cfg(feature = "sync")]
    custom_type.read().visit_children(context);

    #[cfg(not(feature = "sync"))]
    custom_type.borrow().visit_children(context);
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
                #[cfg(feature = "sync")]
                if let Some(watched) = &self.watched {
                    watched.note(&value);
                }
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
        visit_custom_children(&custom_type, self);
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

    // The reachable flag keeps a struct from being walked twice, which is
    // also what ends the walk on a cycle.
    fn visit_mutable_struct(&mut self, steel_struct: MutableStructRef) -> Self::Output {
        let cell = steel_struct.cell();
        if cell.mark_reachable_was_set() {
            return;
        }

        for field in unsafe { cell.fields_unlocked() } {
            self.push_back(field.clone());
        }
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
                if let Some(watched) = &self.watched {
                    watched.note(value);
                }
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
            #[cfg(feature = "sync")]
            watched: None,
        };

        // Intercept the downstream ones, keep them alive by using
        // a local queue first, and then spilling over to the larger one.
        visit_custom_children(custom_type, &mut temporary_queue);

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

    fn visit_mutable_struct(&mut self, cell: MutableStructCell) -> Self::Output {
        if cell.mark_reachable_was_set() {
            return;
        }

        self.stats.memory_reached_count += 1;

        for field in unsafe { cell.fields_unlocked() } {
            self.push_back(field);
        }
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

// A mutable struct's memory belongs to the heap's list of them. The cells go
// from in use, to spare on the thread that emptied one, to dead and freed,
// and these follow one through each of those.
#[cfg(all(test, feature = "sync"))]
mod mutable_struct_list_tests {
    use super::*;

    fn descriptor(n: usize) -> StructTypeDescriptor {
        StructTypeDescriptor::for_test(n)
    }

    fn ints(values: &[isize]) -> impl ExactSizeIterator<Item = SteelVal> {
        values
            .iter()
            .map(|n| SteelVal::IntV(*n))
            .collect::<Vec<_>>()
            .into_iter()
    }

    // Something whose references can be counted from outside.
    fn counted() -> (SteelVal, Gc<crate::values::lists::Pair>) {
        let pair = Gc::new(crate::values::lists::Pair::cons(
            SteelVal::IntV(1),
            SteelVal::IntV(2),
        ));
        (SteelVal::Pair(pair.clone()), pair)
    }

    fn int_at(handle: &MutableStructRef, index: usize) -> isize {
        match handle.get_index(index) {
            Some(SteelVal::IntV(n)) => n,
            other => panic!("expected an integer in field {}, found {:?}", index, other),
        }
    }

    #[test]
    fn a_new_struct_is_in_use_and_holds_what_it_was_given() {
        let mut list = MutableStructList::new();
        let handle = list.allocate(descriptor(3), ints(&[10, 20, 30]));

        assert_eq!(list.cells.len(), 1);
        assert_eq!(handle.cell().state(), MUTABLE_STRUCT_LIVE);
        assert_eq!(handle.len(), 3);
        assert_eq!(handle.descriptor(), descriptor(3));
        assert_eq!(handle.strong_count(), 1);
        assert_eq!(int_at(&handle, 0), 10);
        assert_eq!(int_at(&handle, 2), 30);
        assert!(handle.get_index(3).is_none());
    }

    #[test]
    fn fields_can_be_set_and_hand_back_what_they_held() {
        let mut list = MutableStructList::new();
        let handle = list.allocate(descriptor(1), ints(&[1, 2]));

        assert!(matches!(
            handle.set_index(1, SteelVal::IntV(99)),
            Some(SteelVal::IntV(2))
        ));
        assert_eq!(int_at(&handle, 1), 99);
        assert_eq!(int_at(&handle, 0), 1);
        assert!(handle.set_index(2, SteelVal::IntV(0)).is_none());
    }

    #[test]
    fn handles_share_one_struct() {
        let mut list = MutableStructList::new();
        let handle = list.allocate(descriptor(1), ints(&[1]));
        let other = handle.clone();

        assert!(handle.ptr_eq(&other));
        assert_eq!(handle.strong_count(), 2);
        other.set_index(0, SteelVal::IntV(5));
        assert_eq!(int_at(&handle, 0), 5);

        drop(other);
        assert_eq!(handle.strong_count(), 1);
        assert_eq!(handle.cell().state(), MUTABLE_STRUCT_LIVE);
        assert_eq!(int_at(&handle, 0), 5);
    }

    #[test]
    fn the_last_handle_empties_it_and_keeps_it_spare() {
        let mut list = MutableStructList::new();
        let (value, pair) = counted();
        let handle = list.allocate(descriptor(1), [value, SteelVal::IntV(1)].into_iter());
        let cell = handle.cell();
        assert_eq!(Gc::strong_count(&pair), 2);

        drop(handle);

        assert_eq!(Gc::strong_count(&pair), 1, "what it held was let go of");
        assert_eq!(cell.state(), MUTABLE_STRUCT_SPARE);
        assert_eq!(list.cells.len(), 1, "the heap still has the cell");
    }

    #[test]
    fn a_spare_is_what_the_next_struct_of_its_size_is_made_in() {
        let mut list = MutableStructList::new();
        let first = list.allocate(descriptor(1), ints(&[1, 2]));
        let address = first.as_ptr();
        drop(first);

        // Of another type, which makes no difference to the memory.
        let second = list.allocate(descriptor(2), ints(&[7, 8]));

        assert_eq!(second.as_ptr(), address);
        assert_eq!(list.cells.len(), 1);
        assert_eq!(second.cell().state(), MUTABLE_STRUCT_LIVE);
        assert_eq!(second.descriptor(), descriptor(2));
        assert_eq!(second.strong_count(), 1);
        assert_eq!(int_at(&second, 0), 7);
        assert_eq!(int_at(&second, 1), 8);
    }

    #[test]
    fn a_spare_is_not_used_for_a_struct_of_another_size() {
        let mut list = MutableStructList::new();
        let two = list.allocate(descriptor(1), ints(&[1, 2]));
        let address = two.as_ptr();
        drop(two);

        let three = list.allocate(descriptor(1), ints(&[1, 2, 3]));
        assert_ne!(three.as_ptr(), address);
        assert_eq!(list.cells.len(), 2);

        // The one of two fields is still there for the next of two.
        let two_again = list.allocate(descriptor(1), ints(&[5, 6]));
        assert_eq!(two_again.as_ptr(), address);
        assert_eq!(list.cells.len(), 2);
    }

    #[test]
    fn a_struct_with_no_fields_goes_through_the_same_states() {
        let mut list = MutableStructList::new();
        let empty = list.allocate(descriptor(1), ints(&[]));
        let cell = empty.cell();
        assert_eq!(empty.len(), 0);
        drop(empty);
        assert_eq!(cell.state(), MUTABLE_STRUCT_SPARE);

        let again = list.allocate(descriptor(2), ints(&[]));
        assert_eq!(again.as_ptr(), cell.0.as_ptr() as *const u8);
    }

    #[test]
    fn a_large_struct_is_not_kept_spare() {
        let mut list = MutableStructList::new();
        let fields: Vec<isize> = (0..SPARE_STRUCT_LENGTHS as isize).collect();
        let large = list.allocate(descriptor(1), ints(&fields));
        let cell = large.cell();
        drop(large);

        assert_eq!(cell.state(), MUTABLE_STRUCT_DEAD);
        assert_eq!(list.free_dead(), 1);
        assert!(list.cells.is_empty());
    }

    #[test]
    fn only_so_many_spares_are_kept() {
        let mut list = MutableStructList::new();
        let kept = spare_handles();
        let handles: Vec<_> = (0..kept + 5)
            .map(|n| list.allocate(descriptor(1), ints(&[n as isize])))
            .collect();
        let cells: Vec<_> = handles.iter().map(|handle| handle.cell()).collect();
        drop(handles);

        let spare = cells
            .iter()
            .filter(|cell| cell.state() == MUTABLE_STRUCT_SPARE)
            .count();
        let dead = cells
            .iter()
            .filter(|cell| cell.state() == MUTABLE_STRUCT_DEAD)
            .count();
        assert_eq!(spare, kept);
        assert_eq!(dead, 5);
    }

    #[test]
    fn freeing_the_dead_takes_the_spares_too() {
        let mut list = MutableStructList::new();
        let handles: Vec<_> = (0..4)
            .map(|n| list.allocate(descriptor(1), ints(&[n])))
            .collect();
        let kept = handles[0].clone();
        drop(handles);
        assert_eq!(list.cells.len(), 4);

        // Three were spare and one is in use.
        assert_eq!(list.free_dead(), 3);
        assert_eq!(list.cells.len(), 1);
        assert_eq!(kept.cell().state(), MUTABLE_STRUCT_LIVE);
        assert_eq!(int_at(&kept, 0), 0);

        // With no spare left the next one is new memory.
        let next = list.allocate(descriptor(1), ints(&[9]));
        assert_eq!(list.cells.len(), 2);
        assert_eq!(int_at(&next, 0), 9);
    }

    #[test]
    fn a_cycle_is_only_found_by_what_the_mark_did_not_reach() {
        let mut list = MutableStructList::new();
        let a = list.allocate(descriptor(1), ints(&[0]));
        let b = list.allocate(descriptor(1), ints(&[0]));
        a.set_index(0, SteelVal::MutableStruct(b.clone()));
        b.set_index(0, SteelVal::MutableStruct(a.clone()));
        let (a_cell, b_cell) = (a.cell(), b.cell());

        drop(a);
        drop(b);
        // Each is held by the other, so neither was emptied.
        assert_eq!(a_cell.state(), MUTABLE_STRUCT_LIVE);
        assert_eq!(b_cell.state(), MUTABLE_STRUCT_LIVE);

        // A mark that reaches neither.
        list.mark_all_unreachable();
        let released = list.take_unreachable();
        assert_eq!(released.len(), 2);
        drop(released);

        // Letting go of what they held let go of them.
        assert_ne!(a_cell.state(), MUTABLE_STRUCT_LIVE);
        assert_ne!(b_cell.state(), MUTABLE_STRUCT_LIVE);
        assert_eq!(list.free_dead(), 2);
        assert!(list.cells.is_empty());
    }

    #[test]
    fn what_the_mark_reached_is_left_alone() {
        let mut list = MutableStructList::new();
        let reached = list.allocate(descriptor(1), ints(&[1]));
        let missed = list.allocate(descriptor(1), ints(&[2]));

        list.mark_all_unreachable();
        assert!(!reached.cell().is_reachable());
        assert!(
            !reached.cell().mark_reachable_was_set(),
            "the first mark finds it unset"
        );
        assert!(reached.cell().mark_reachable_was_set(), "and sets it");

        let released = list.take_unreachable();
        assert_eq!(released.len(), 1);
        assert_eq!(int_at(&reached, 0), 1);
        // A struct that was missed while something still has a handle to it
        // has been emptied, and reads as holding nothing.
        assert!(matches!(missed.get_index(0), Some(SteelVal::Void)));
    }

    #[test]
    fn one_heaps_spare_is_not_used_by_another() {
        let mut first = MutableStructList::new();
        let mut second = MutableStructList::new();

        let handle = first.allocate(descriptor(1), ints(&[1, 2]));
        let cell = handle.cell();
        drop(handle);
        assert_eq!(cell.state(), MUTABLE_STRUCT_SPARE);

        let other = second.allocate(descriptor(1), ints(&[3, 4]));
        assert_ne!(other.as_ptr(), cell.0.as_ptr() as *const u8);
        assert_eq!(second.cells.len(), 1);
        // It was handed back to the heap it came from, to free.
        assert_eq!(cell.state(), MUTABLE_STRUCT_DEAD);
        assert_eq!(first.free_dead(), 1);
    }

    #[test]
    fn a_handle_that_outlives_its_heap_is_safe_and_empty() {
        let (value, pair) = counted();
        let handle = {
            let mut list = MutableStructList::new();
            list.allocate(descriptor(1), [value, SteelVal::IntV(7)].into_iter())
        };

        // A heap that goes away empties what it owns, since a cycle among
        // them could not be found afterwards. The handle is still good to
        // use, on a struct that holds nothing.
        assert_eq!(Gc::strong_count(&pair), 1);
        assert_eq!(handle.len(), 2);
        assert!(matches!(handle.get_index(0), Some(SteelVal::Void)));
        assert!(matches!(handle.get_index(1), Some(SteelVal::Void)));

        handle.set_index(1, SteelVal::IntV(8));
        assert_eq!(int_at(&handle, 1), 8);
        let other = handle.clone();
        assert_eq!(other.strong_count(), 2);
        drop(other);

        // With no heap to keep the memory for, the last handle frees it.
        drop(handle);
    }

    #[test]
    fn a_heap_that_goes_away_lets_go_of_what_its_structs_hold() {
        let (value, pair) = counted();
        {
            let mut list = MutableStructList::new();
            let a = list.allocate(descriptor(1), [value, SteelVal::IntV(0)].into_iter());
            let b = list.allocate(descriptor(1), ints(&[0, 0]));
            // A cycle, which no handle going away would have found.
            a.set_index(1, SteelVal::MutableStruct(b.clone()));
            b.set_index(1, SteelVal::MutableStruct(a.clone()));
        }
        assert_eq!(Gc::strong_count(&pair), 1);
    }
}
