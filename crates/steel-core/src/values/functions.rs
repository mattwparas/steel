#![allow(unused)]

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    convert::TryFrom,
    hash::Hasher,
    sync::Arc,
};

use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};

use crate::{
    compiler::code_gen::fresh_function_id,
    core::{
        instructions::{u24, DenseInstruction},
        opcode::OpCode,
    },
    gc::{
        shared::{MutContainer, ShareableMut, StandardShared},
        Gc, Shared, SharedMut,
    },
    parser::{parser::SyntaxObjectId, span::Span},
    rvals::{
        from_serializable_value, into_serializable_value, AsRefSteelVal, Custom, FunctionSignature,
        HeapSerializer, IntoSteelVal, MutFunctionSignature, SerializableSteelVal, SteelString,
        SteelValGeneric,
    },
    steel_vm::{
        register_fn::SendSyncStatic,
        vm::{BlockMetadata, BlockPattern, BuiltInSignature},
    },
    SteelErr, SteelVal,
};

use super::{
    closed::{Heap, HeapRef},
    structs::UserDefinedStruct,
};

// Keeps track of this metadata table for getting the docs associated
// with a given function
#[derive(Clone)]
pub struct LambdaMetadataTable {
    fn_ptr_table: HashMap<usize, SteelString>,
}

// Note: If this is getting deserialized, we need something better than
// just the function pointer to this, since its possible that the function
// pointer now changes across images. We'll have to reconstruct it
// on bootup.
//
// What that might mean is we record the pointer address of every function
// pointer, and then when re initializating, we map that back to the value
// that it had later.
impl Custom for LambdaMetadataTable {
    fn into_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        // Some(SerializableSteelVal::Custom(Box::new(self.clone())))

        None
    }
}

impl LambdaMetadataTable {
    pub fn new() -> Self {
        Self {
            fn_ptr_table: HashMap::new(),
        }
    }

    pub fn add(&mut self, function: SteelVal, doc: SteelString) {
        match function {
            SteelVal::Closure(b) => {
                self.fn_ptr_table.insert(b.id as _, doc);
            }
            SteelVal::BoxedFunction(b) => {
                self.fn_ptr_table.insert(Gc::as_ptr(&b) as usize, doc);
            }
            _ => {}
        }
    }

    pub fn get(&self, function: SteelVal) -> Option<SteelString> {
        match function {
            SteelVal::Closure(b) => self.fn_ptr_table.get(&(b.id as _)).cloned(),
            SteelVal::BoxedFunction(b) => {
                self.fn_ptr_table.get(&(Gc::as_ptr(&b) as usize)).cloned()
            }
            _ => None,
        }
    }

    // TODO: This will need to get called in other places
    pub fn collect_garbage(&mut self, keep_set: impl Iterator<Item = usize>) {
        let set = keep_set.collect::<std::collections::HashSet<_>>();

        self.fn_ptr_table.retain(|k, _| set.contains(k));
    }
}

#[cfg(feature = "inline-captures")]
const INLINE_CAPTURE_SIZE: usize = 3;

// `CaptureVec<A>` is cfg-split the same way `Gc<T>`/`Gc<T, A>` is (see gc.rs): only under
// `sync+biased+allocator-api2` does a closure's captured variables actually get allocated
// through `A` rather than always through `Global`. `inline-captures` (an opt-in,
// non-default feature) keeps using plain `smallvec` regardless -- its inline storage never
// allocates at all, and its heap-spill path has no allocator hook in mainline `smallvec`
// (see ALLOCATOR_SPEC.md §3.6); that's an orthogonal, already-accepted limitation, not
// something this phase changes.
#[cfg(all(
    not(feature = "inline-captures"),
    not(all(
        feature = "sync",
        feature = "biased",
        feature = "allocator-api2",
        not(feature = "triomphe")
    ))
))]
pub type CaptureVec<A = crate::gc::Global> = Vec<SteelValGeneric<A>>;

#[cfg(feature = "inline-captures")]
pub type CaptureVec<A = crate::gc::Global> = smallvec::SmallVec<[SteelValGeneric<A>; INLINE_CAPTURE_SIZE]>;

#[cfg(all(
    not(feature = "inline-captures"),
    feature = "sync",
    feature = "biased",
    feature = "allocator-api2",
    not(feature = "triomphe")
))]
pub type CaptureVec<A = crate::gc::Global> = allocator_api2::vec::Vec<SteelValGeneric<A>, A>;

/// Take the captures out of a `CaptureVec<A>`, leaving an empty one behind. Plain
/// `mem::take` needs `CaptureVec<A>: Default`, which the gated `allocator_api2::vec::Vec<T,
/// A>` backing only has for `A: Default` (i.e. `Global`) -- a real custom allocator
/// generally isn't `Default`. This builds the replacement from the same allocator instance
/// the original buffer already held, so it works for any `A`.
#[cfg(all(
    not(feature = "inline-captures"),
    feature = "sync",
    feature = "biased",
    feature = "allocator-api2",
    not(feature = "triomphe")
))]
pub(crate) fn take_captures<A: crate::gc::Allocator + Clone + Send + Sync + 'static>(
    captures: &mut CaptureVec<A>,
) -> CaptureVec<A> {
    let alloc = captures.allocator().clone();
    core::mem::replace(captures, allocator_api2::vec::Vec::new_in(alloc))
}

#[cfg(not(all(
    not(feature = "inline-captures"),
    feature = "sync",
    feature = "biased",
    feature = "allocator-api2",
    not(feature = "triomphe")
)))]
pub(crate) fn take_captures<A: crate::gc::Allocator + Clone + Send + Sync + 'static>(
    captures: &mut CaptureVec<A>,
) -> CaptureVec<A> {
    core::mem::take(captures)
}

#[cfg(all(
    not(feature = "inline-captures"),
    feature = "sync",
    feature = "biased",
    feature = "allocator-api2",
    not(feature = "triomphe")
))]
pub(crate) fn empty_captures_in<A: crate::gc::Allocator + Clone + Send + Sync + 'static>(alloc: A) -> CaptureVec<A> {
    allocator_api2::vec::Vec::new_in(alloc)
}

#[cfg(not(all(
    not(feature = "inline-captures"),
    feature = "sync",
    feature = "biased",
    feature = "allocator-api2",
    not(feature = "triomphe")
)))]
pub(crate) fn empty_captures_in<A: crate::gc::Allocator + Clone + Send + Sync + 'static>(
    _alloc: A,
) -> CaptureVec<A> {
    CaptureVec::default()
}

/// Same as `empty_captures_in`, but pre-reserves `capacity` slots -- used when the number of
/// captured variables is known up front (closure construction in the VM's inner loop).
#[cfg(all(
    not(feature = "inline-captures"),
    feature = "sync",
    feature = "biased",
    feature = "allocator-api2",
    not(feature = "triomphe")
))]
pub(crate) fn captures_with_capacity_in<A: crate::gc::Allocator + Clone + Send + Sync + 'static>(
    capacity: usize,
    alloc: A,
) -> CaptureVec<A> {
    allocator_api2::vec::Vec::with_capacity_in(capacity, alloc)
}

#[cfg(not(all(
    not(feature = "inline-captures"),
    feature = "sync",
    feature = "biased",
    feature = "allocator-api2",
    not(feature = "triomphe")
)))]
pub(crate) fn captures_with_capacity_in<A: crate::gc::Allocator + Clone + Send + Sync + 'static>(
    capacity: usize,
    _alloc: A,
) -> CaptureVec<A> {
    CaptureVec::with_capacity(capacity)
}

// A closure's own heap block is squarely in scope for allocator-routing (unlike, say, a
// persistent Vector/HashMap's internal node storage, deferred to a later phase per
// ALLOCATOR_SPEC.md) -- creating a closure is one of the hottest, most frequent allocations
// a running program makes. `Gc<T, A>` (the 2-parameter form) only exists at all under
// `sync+biased+allocator-api2`, so this alias picks the right arity per cfg, the same way
// `SteelString`/`Env` do.
#[cfg(all(
    feature = "sync",
    feature = "biased",
    feature = "allocator-api2",
    not(feature = "triomphe")
))]
pub type ByteCodeLambdaGc<A> = Gc<ByteCodeLambda<A>, A>;

#[cfg(not(all(
    feature = "sync",
    feature = "biased",
    feature = "allocator-api2",
    not(feature = "triomphe")
)))]
pub type ByteCodeLambdaGc<A> = Gc<ByteCodeLambda<A>>;

#[derive(Clone)]
pub struct ByteCodeLambda<A: crate::gc::Allocator + Clone + Send + Sync + 'static = crate::gc::Global> {
    pub(crate) id: u32,
    /// body of the function with identifiers yet to be bound
    #[cfg(feature = "dynamic")]
    pub(crate) body_exp: RefCell<Shared<[DenseInstruction]>>,

    #[cfg(not(feature = "dynamic"))]
    pub(crate) body_exp: StandardShared<[DenseInstruction]>,

    pub(crate) arity: u16,

    #[cfg(feature = "dynamic")]
    call_count: Cell<usize>,

    pub(crate) is_multi_arity: bool,

    // Store... some amount inline?
    // pub(crate) captures: Vec<SteelVal>,
    pub(crate) captures: CaptureVec<A>,

    // pub(crate) captures: Box<[SteelVal]>
    #[cfg(feature = "dynamic")]
    pub(crate) blocks: RefCell<Vec<(BlockPattern, BlockMetadata)>>,

    // This is a little suspicious, but it should give us the necessary information to attach a struct of metadata
    #[cfg(feature = "sync")]
    contract: SharedMut<Option<Gc<UserDefinedStruct>>>,

    #[cfg(not(feature = "sync"))]
    contract: MutContainer<Option<Gc<UserDefinedStruct>>>,

    #[cfg(feature = "jit2")]
    pub(crate) super_instructions: Option<fn(&mut crate::steel_vm::vm::VmCore)>,

    // In the event this is serialized and its been jit compiled, replace
    // the first instruction with this, since this is what is was originally
    pub(crate) header: Option<OpCode>,
}

// Not derived: a derived `Debug` would add an `A: Debug` bound to the whole impl even
// though only `id`/`arity`/`is_multi_arity` are actually printed -- `Global` happens to be
// `Debug`, but a real custom allocator (an arena, a ring buffer, ...) generally isn't, and
// there's no reason to require it just to print a closure.
impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> core::fmt::Debug for ByteCodeLambda<A> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ByteCodeLambda")
            .field("id", &self.id)
            .field("arity", &self.arity)
            .field("is_multi_arity", &self.is_multi_arity)
            .finish()
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> PartialEq for ByteCodeLambda<A> {
    fn eq(&self, other: &Self) -> bool {
        // self.body_exp == other.body_exp &&
        self.arity == other.arity && self.id == other.id
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Eq for ByteCodeLambda<A> {}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> core::hash::Hash for ByteCodeLambda<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        // self.body_exp.as_ptr().hash(state);
        self.arity.hash(state);

        // self.sub_expression_env.as_ptr().hash(state);
    }
}

// Can this be moved across threads? What does it cost to execute a closure in another thread?
// Engine instances be deep cloned?
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializedLambda {
    pub id: u32,
    pub body_exp: Vec<DenseInstruction>,
    pub arity: usize,
    pub is_multi_arity: bool,
    // TODO: Go ahead and create a ThreadSafeSteelVal where we will just deep clone everything, move
    // it across the thread, and reconstruct on the other side.
    pub captures: Vec<SerializableSteelVal>,

    pub constants: HashMap<usize, SerializableSteelVal>,
}

#[derive(Clone)]
pub struct SerializedLambdaPrototype {
    pub id: u32,
    pub body_exp: Vec<DenseInstruction>,
    pub arity: usize,
    pub is_multi_arity: bool,
    pub constants: HashMap<usize, SerializableSteelVal>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct RootedInstructions {
    #[cfg(feature = "rooted-instructions")]
    inner: *const [DenseInstruction],
    #[cfg(not(feature = "rooted-instructions"))]
    inner: StandardShared<[DenseInstruction]>,
}

#[cfg(feature = "rooted-instructions")]
impl Copy for RootedInstructions {}

// TODO: Come back to this
unsafe impl Send for RootedInstructions {}
unsafe impl Sync for RootedInstructions {}

impl RootedInstructions {
    pub fn new(instructions: StandardShared<[DenseInstruction]>) -> Self {
        Self {
            #[cfg(feature = "rooted-instructions")]
            inner: StandardShared::as_ptr(&instructions),
            #[cfg(not(feature = "rooted-instructions"))]
            inner: instructions,
        }
    }
}

impl core::fmt::Debug for RootedInstructions {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self.inner)
    }
}

impl core::ops::Deref for RootedInstructions {
    type Target = [DenseInstruction];

    fn deref(&self) -> &Self::Target {
        #[cfg(feature = "rooted-instructions")]
        unsafe {
            &(*self.inner)
        }

        #[cfg(not(feature = "rooted-instructions"))]
        &self.inner
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> ByteCodeLambda<A> {
    pub fn new(
        id: u32,
        body_exp: StandardShared<[DenseInstruction]>,
        arity: usize,
        is_multi_arity: bool,
        captures: CaptureVec<A>,
    ) -> ByteCodeLambda<A> {
        ByteCodeLambda {
            id,

            #[cfg(feature = "dynamic")]
            body_exp: RefCell::new(body_exp),
            #[cfg(not(feature = "dynamic"))]
            body_exp,

            arity: arity as u16,

            #[cfg(feature = "dynamic")]
            call_count: Cell::new(0),

            is_multi_arity,
            captures,

            #[cfg(feature = "sync")]
            contract: SharedMut::new(MutContainer::new(None)),

            #[cfg(not(feature = "sync"))]
            contract: MutContainer::new(None),

            #[cfg(feature = "dynamic")]
            blocks: RefCell::new(Vec::new()),

            #[cfg(feature = "jit2")]
            super_instructions: None,

            header: None,
        }
    }
}

// Deserialization, and the two "no captures yet" bootstrap constructors below, always
// produce concrete, `Global`-backed content (deserialized values, and empty capture
// vectors respectively) -- matching the wider design where compile/load-time machinery
// stays on `Global` and only a running program's own allocations route through `A` (see
// ALLOCATOR_SPEC.md). `CaptureVec::default()` specifically requires this: an empty
// `allocator_api2::vec::Vec<T, A>` needs an allocator instance to be constructed, which
// only `Global` can provide "for free".
impl ByteCodeLambda<crate::gc::Global> {
    pub(crate) fn from_serialized(
        heap: &mut HeapSerializer,
        mut value: SerializedLambda,
    ) -> Result<Self, SteelErr> {
        // Map the old to the new
        let id = fresh_function_id();
        heap.function_mapping.insert(value.id, id as _);

        let mut new_body = Vec::with_capacity(value.body_exp.len());
        let mut closures_to_rewrite = Vec::new();

        for (idx, instr) in value.body_exp.iter().enumerate() {
            let mut instr = *instr;

            // TODO: Rewrite the closures these reference as well.
            // If there is an SCLOSURE or something, then a new closure
            // will get created (or maybe, was already created) that pointed
            // to the old one?
            match instr.op_code {
                // If this instruction touches this global variable,
                // then we want to mark it as possibly referenced here.
                OpCode::CALLGLOBAL
                | OpCode::CALLPRIMITIVE
                | OpCode::PUSH
                | OpCode::CALLGLOBALTAIL
                | OpCode::CALLGLOBALNOARITY
                | OpCode::CALLGLOBALTAILNOARITY => {
                    instr.payload_size = u24::from_usize(
                        *heap
                            .global_mapping
                            .get(&instr.payload_size.to_usize())
                            .unwrap(),
                    );
                }

                OpCode::PUSHCONST => {
                    let old_index = instr.payload_size.to_usize();
                    let old_value = value.constants.get(&old_index).cloned().unwrap();
                    let deserialized_constant = from_serializable_value(heap, old_value)?;
                    let new_index = heap
                        .thread
                        .compiler
                        .write()
                        .constant_map
                        .add_or_get(deserialized_constant);
                    instr.payload_size = u24::from_usize(new_index);
                }

                // TODO: Find the ip of the closure, and then go allocate a _new_ closure
                // for this one, since the values are going to now be rewritten.
                OpCode::NEWSCLOSURE => {
                    // closure IP
                    let closure_id = value.body_exp.get(idx + 2).unwrap().payload_size.to_u32();

                    if let Some(old) = heap.function_mapping.get(&closure_id) {
                        closures_to_rewrite.push((idx + 2, *old));
                    } else {
                        closures_to_rewrite.push((idx + 2, fresh_function_id() as _));
                    }
                }
                _ => {}
            }

            new_body.push(instr);
        }

        for (idx, id) in closures_to_rewrite {
            new_body[idx].payload_size = u24::from_u32(id);
        }

        Ok(ByteCodeLambda::new(
            value.id,
            new_body.into(),
            value.arity,
            value.is_multi_arity,
            value
                .captures
                .into_iter()
                .map(|x| from_serializable_value(heap, x))
                .collect::<Result<_, _>>()?,
        ))
    }

    pub fn rooted(instructions: StandardShared<[DenseInstruction]>) -> ByteCodeLambda {
        Self::new(
            SyntaxObjectId::fresh().into(),
            instructions,
            0,
            false,
            CaptureVec::default(),
        )
    }

    pub fn main(instructions: Vec<DenseInstruction>) -> ByteCodeLambda {
        Self::new(
            SyntaxObjectId::fresh().into(),
            instructions.into(),
            0,
            false,
            CaptureVec::default(),
        )
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> ByteCodeLambda<A> {
    /// The `A`-generic counterpart to `rooted`/`main` (which need `CaptureVec::default()`,
    /// only available for `Global`, same as `Gc::new`). Used for the bootstrap/"no captures
    /// yet" closures `SteelThread<A>::execute` roots the running instructions to.
    pub fn rooted_in(instructions: StandardShared<[DenseInstruction]>, alloc: A) -> Self {
        Self::new(
            SyntaxObjectId::fresh().into(),
            instructions,
            0,
            false,
            empty_captures_in(alloc),
        )
    }

    pub fn set_captures(&mut self, captures: CaptureVec<A>) {
        self.captures = captures;
    }

    // TODO: The lifecycle of `RootedInstructions` should not be
    // beyond the scope of execution. This invariant should in
    // general hold - with the exception of continuations, which
    // should probably hold on to any functions that are contains
    // strongly - so there should be some kind of slot on the continuation
    // to hold on to a strong reference to each instruction set.
    pub(crate) fn body_exp(&self) -> RootedInstructions {
        // #[cfg(feature = "dynamic")]
        // return Shared::clone(&self.body_exp.borrow());

        // #[cfg(not(feature = "dynamic"))]
        // Shared::clone(&self.body_exp)

        #[cfg(not(feature = "rooted-instructions"))]
        return RootedInstructions {
            inner: StandardShared::clone(&self.body_exp),
        };

        #[cfg(feature = "rooted-instructions")]
        return RootedInstructions {
            inner: StandardShared::as_ptr(&self.body_exp),
        };
    }

    pub fn body_mut_exp(&mut self) -> StandardShared<[DenseInstruction]> {
        #[cfg(feature = "dynamic")]
        return StandardShared::clone(self.body_exp.get_mut());

        #[cfg(not(feature = "dynamic"))]
        StandardShared::clone(&self.body_exp)
    }

    // pub fn spans(&self) -> Rc<[Span]> {
    //     Rc::clone(&self.spans)
    // }

    // Get the starting index in the instruction set, and the new ID to associate with this
    // super instruction set.
    // Deep copy the old instruction set, update the new spot to have a dynamic super instruction
    // associated with it.
    #[cfg(feature = "dynamic")]
    pub fn update_to_super_instruction(
        &self,
        start: usize,
        super_instruction_id: usize,
    ) -> (DenseInstruction, Shared<[DenseInstruction]>) {
        let mut guard = self.body_exp.borrow_mut();
        let mut old: Box<[_]> = guard.iter().copied().collect();

        // set up the head instruction to get returned, we'll need it in the block first
        let head_instruction = old[start];

        // Point to the new super instruction
        old[start].op_code = OpCode::DynSuperInstruction;
        old[start].payload_size = super_instruction_id as _;
        *guard = old.into();
        (head_instruction, Shared::clone(&guard))
    }

    #[inline(always)]
    pub fn arity(&self) -> usize {
        self.arity as usize
    }

    #[inline(always)]
    pub fn is_multi_arity(&self) -> bool {
        self.is_multi_arity
    }

    // pub fn heap_allocated(&self) -> &RefCell<Vec<HeapRef<SteelVal>>> {
    //     &self.heap_allocated
    // }

    pub fn captures(&self) -> &[SteelValGeneric<A>] {
        &self.captures
    }

    #[cfg(feature = "dynamic")]
    #[inline(always)]
    pub fn increment_call_count(&self) {
        // self.call_count += 1;
        self.call_count.set(self.call_count.get() + 1);
    }

    #[cfg(feature = "dynamic")]
    pub fn call_count(&self) -> usize {
        self.call_count.get()
    }

    // pub fn set_cannot_be_compiled(&self) {
    //     self.cant_be_compiled.set(true)
    // }

    // pub fn has_attempted_to_be_compiled(&self) -> bool {
    //     self.cant_be_compiled.get()
    // }

    pub fn attach_contract_information(&self, steel_struct: Gc<UserDefinedStruct>) {
        #[cfg(feature = "sync")]
        {
            let mut guard = self.contract.write();

            *guard = Some(steel_struct);
        }

        #[cfg(not(feature = "sync"))]
        {
            let mut guard = self.contract.borrow_mut();

            *guard = Some(steel_struct);
        }
    }

    pub fn get_contract_information(&self) -> Option<SteelVal> {
        #[cfg(feature = "sync")]
        {
            self.contract
                .read()
                .as_ref()
                .map(|x| SteelVal::CustomStruct(x.clone()))
        }

        #[cfg(not(feature = "sync"))]
        {
            self.contract
                .borrow()
                .as_ref()
                .map(|x| SteelVal::CustomStruct(x.clone()))
        }
    }

    // pub fn mark_hot(&self) {
    //     self.is_hot.set(true)
    // }

    // pub(crate) fn mark_block_tail(&self, pattern: BlockPattern) {
    //     self.blocks.borrow_mut();
    // }

    // pub(crate) fn check_tail(&self, pattern: &BlockPattern) -> bool {
    //     self.blocks.borrow().contains(pattern)
    // }

    // pub(crate) fn block_tail(&self, block_pattern
}

pub fn attach_contract_struct(args: &[SteelVal]) -> crate::rvals::Result<SteelVal> {
    if let SteelVal::Closure(closure) = &args[0] {
        if let SteelVal::CustomStruct(s) = &args[1] {
            closure.attach_contract_information(s.clone());

            Ok(SteelVal::Void)
        } else {
            stop!(TypeMismatch => "attach-contract-struct! expects a struct in the second position")
        }
    } else {
        stop!(TypeMismatch => "attach-contract-struct! expects a function in the first position")
    }
}

pub fn get_contract(args: &[SteelVal]) -> crate::rvals::Result<SteelVal> {
    if let SteelVal::Closure(closure) = &args[0] {
        closure.get_contract_information().into_steelval()
    } else {
        Ok(SteelVal::BoolV(false))

        // stop!(TypeMismatch => "get-contract-struct! expects a function in the first position, found: {}", &args[0])
    }
}

#[derive(Clone)]
#[repr(C)]
pub enum StaticOrRcStr {
    Static(&'static str),
    Owned(Arc<String>),
}

/// This allows cloning the underlying closure, so we can send it across threads.
/// It does _not_ solve serializing closures fully, but it does mean we can move function
/// pointers across threads, which should be very helpful with spawning native threads.
// TODO: @Matt - Replace usage of BoxedDynFunction (and subsequent call sites) with this instead
// trait DynamicFunction: Send + Sync {
//     #[inline]
//     fn call(&self, args: &[SteelVal]) -> crate::rvals::Result<SteelVal>;
//     fn clone_box(&self) -> Box<dyn DynamicFunction>;
// }

// // Allow only the capturing of send + sync variables?
// impl<F: Fn(&[SteelVal]) -> crate::rvals::Result<SteelVal> + Clone + Send + Sync + 'static>
//     DynamicFunction for F
// {
//     fn call(&self, args: &[SteelVal]) -> crate::rvals::Result<SteelVal> {
//         (self)(args)
//     }

//     fn clone_box(&self) -> Box<dyn DynamicFunction> {
//         Box::new(self.clone())
//     }
// }

// impl Clone for Box<dyn DynamicFunction> {
//     fn clone(&self) -> Self {
//         self.clone_box()
//     }
// }

// pub enum MaybeSendSyncFunction {}

#[derive(Clone)]
#[repr(C)]
pub struct BoxedDynFunction {
    pub function:
        Arc<dyn Fn(&[SteelVal]) -> crate::rvals::Result<SteelVal> + Send + Sync + 'static>,
    pub name: Option<Arc<String>>,
    pub arity: Option<u32>,
}

impl BoxedDynFunction {
    // pub fn spawn_on_thread(self) {
    //     std::thread::spawn(move || self.function);
    // }

    pub(crate) fn new(
        function: Arc<
            dyn Fn(&[SteelVal]) -> crate::rvals::Result<SteelVal> + Send + Sync + 'static,
        >,
        name: Option<&str>,
        arity: Option<u32>,
    ) -> Self {
        BoxedDynFunction {
            function,
            name: name.map(|x| Arc::new(x.to_string())),
            arity,
        }
    }

    pub(crate) fn new_owned(
        function: Arc<
            dyn Fn(&[SteelVal]) -> crate::rvals::Result<SteelVal> + Send + Sync + 'static,
        >,
        name: Option<Arc<String>>,
        arity: Option<u32>,
    ) -> Self {
        BoxedDynFunction {
            function,
            name,
            arity,
        }
    }

    #[inline(always)]
    pub fn func(
        &self,
    ) -> &(dyn Fn(&[SteelVal]) -> crate::rvals::Result<SteelVal> + Send + Sync + 'static) {
        self.function.as_ref()
    }

    #[inline(always)]
    pub fn get_arity(&self) -> Option<u32> {
        self.arity
    }

    #[inline(always)]
    pub fn name(&self) -> Option<&str> {
        self.name.as_ref().map(|x| x.as_str())
    }
}
