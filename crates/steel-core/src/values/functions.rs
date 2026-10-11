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

#[cfg(feature = "jit2")]
use crate::jit2::cgen::JitFnPointer;
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
    },
    steel_vm::{
        register_fn::SendSyncStatic,
        vm::{BlockMetadata, BlockPattern, BuiltInSignature, InstructionPointer},
    },
    SteelErr, SteelVal,
};

use super::{
    closed::{Heap, HeapRef},
    structs::{StructRef, UserDefinedStruct},
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

#[cfg(not(feature = "inline-captures"))]
pub type CaptureVec = steel_vec::Vec<SteelVal>;

#[cfg(feature = "inline-captures")]
pub type CaptureVec = smallvec::SmallVec<[SteelVal; INLINE_CAPTURE_SIZE]>;

#[repr(C)]
#[derive(Clone, Debug)]
pub struct ByteCodeLambda {
    pub(crate) id: u32,
    pub(crate) arity: u16,
    pub(crate) is_multi_arity: bool,
    // In the event this is serialized and its been jit compiled, replace
    // the first instruction with this, since this is what is was originally
    pub(crate) header: Option<OpCode>,

    /// body of the function with identifiers yet to be bound
    #[cfg(feature = "dynamic")]
    pub(crate) body_exp: RefCell<Shared<[DenseInstruction]>>,

    // #[cfg(not(feature = "dynamic"))]
    // pub(crate) body_exp: StandardShared<[DenseInstruction]>,
    #[cfg(not(feature = "dynamic"))]
    pub(crate) body_exp: InstructionPointer<[DenseInstruction]>,

    #[cfg(feature = "dynamic")]
    call_count: Cell<usize>,

    // TODO: Make this a vector which can be access from the JIT.
    pub(crate) captures: CaptureVec,

    // pub(crate) captures: Box<[SteelVal]>
    #[cfg(feature = "dynamic")]
    pub(crate) blocks: RefCell<Vec<(BlockPattern, BlockMetadata)>>,

    // This is a little suspicious, but it should give us the necessary information to attach a struct of metadata
    #[cfg(feature = "sync")]
    contract: SharedMut<Option<StructRef>>,

    #[cfg(not(feature = "sync"))]
    contract: MutContainer<Option<StructRef>>,

    /// The compiled entry point, or null when not compiled.
    #[cfg(feature = "jit2")]
    pub(crate) super_instructions: JitEntry,

    #[cfg(feature = "jit2")]
    pub(crate) tier2: Option<JitFnPointer>,
}

/// A lambda's compiled entry point, null until compiled.
///
/// Atomic because the background compiler installs it into a lambda the
/// interpreter already holds (see `jit_background_enabled`). It has the same
/// layout as `Option<JitFnPointer>`, a single pointer that is null for none,
/// which the generated code loads through
/// `offset_of!(ByteCodeLambda, super_instructions)`.
#[cfg(feature = "jit2")]
#[repr(transparent)]
pub(crate) struct JitEntry(core::sync::atomic::AtomicPtr<u8>);

#[cfg(feature = "jit2")]
impl JitEntry {
    pub(crate) fn new(pointer: Option<JitFnPointer>) -> Self {
        let entry = Self(core::sync::atomic::AtomicPtr::new(core::ptr::null_mut()));
        entry.store(pointer);
        entry
    }

    #[inline]
    pub(crate) fn load(&self) -> Option<JitFnPointer> {
        core::ptr::NonNull::new(self.0.load(core::sync::atomic::Ordering::Acquire))
            .map(JitFnPointer)
    }

    #[inline]
    pub(crate) fn store(&self, pointer: Option<JitFnPointer>) {
        let raw = pointer.map_or(core::ptr::null_mut(), |p| p.0.as_ptr());
        self.0.store(raw, core::sync::atomic::Ordering::Release);
    }
}

#[cfg(feature = "jit2")]
impl Clone for JitEntry {
    fn clone(&self) -> Self {
        Self::new(self.load())
    }
}

#[cfg(feature = "jit2")]
impl core::fmt::Debug for JitEntry {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self.load())
    }
}

#[cfg(feature = "jit2")]
const _: () =
    assert!(core::mem::size_of::<JitEntry>() == core::mem::size_of::<Option<JitFnPointer>>());

impl Default for ByteCodeLambda {
    fn default() -> Self {
        Self {
            id: 0,
            arity: 0,
            is_multi_arity: false,
            header: None,
            body_exp: Vec::new().into(),
            #[cfg(feature = "sync")]
            contract: SharedMut::new(MutContainer::new(None)),
            #[cfg(not(feature = "sync"))]
            contract: MutContainer::new(None),
            captures: Default::default(),
            #[cfg(feature = "jit2")]
            super_instructions: JitEntry::new(None),

            #[cfg(feature = "jit2")]
            tier2: None,
        }
    }
}

impl PartialEq for ByteCodeLambda {
    fn eq(&self, other: &Self) -> bool {
        // self.body_exp == other.body_exp &&
        self.arity == other.arity && self.id == other.id
    }
}

impl Eq for ByteCodeLambda {}

impl core::hash::Hash for ByteCodeLambda {
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
    // What it closes over, and the constants its body refers to by their
    // index in the constant map, as places in the serialized value's table.
    pub captures: Vec<crate::rvals::SerializedId>,

    pub constants: HashMap<usize, crate::rvals::SerializedId>,
}

/// What the number an instruction carries refers to, where that is
/// something a function has to have with it when it is sent, or a place in
/// the function.
pub(crate) enum Operand {
    // The index of a global.
    Global,
    // The index of a constant.
    Constant,
    // The index of the instruction to go on from.
    Jump,
    // How far on the function this makes ends.
    Closure,
    // The index of something the function closes over.
    Captured,
    Other,
}

impl Operand {
    pub(crate) fn of(op_code: OpCode) -> Operand {
        match op_code {
            OpCode::PUSH
            | OpCode::SET
            | OpCode::BIND
            | OpCode::CALLGLOBAL
            | OpCode::CALLGLOBALTAIL
            | OpCode::CALLGLOBALNOARITY
            | OpCode::CALLGLOBALTAILNOARITY
            | OpCode::CALLPRIMITIVE
            | OpCode::CALLPRIMITIVETAIL => Operand::Global,
            OpCode::PUSHCONST | OpCode::EQUALCONST => Operand::Constant,
            OpCode::IF | OpCode::JMP => Operand::Jump,
            OpCode::NEWSCLOSURE | OpCode::PUREFUNC => Operand::Closure,
            OpCode::READCAPTURED => Operand::Captured,
            _ => Operand::Other,
        }
    }
}

impl SerializedLambda {
    /// Checks that the body hangs together: that it refers to constants it
    /// was sent with and captures it has, that jumps land inside it, and
    /// that the functions it makes are laid out as the interpreter reads
    /// them. A body that came from bytes can be anything.
    ///
    /// This is not a verifier. What an instruction finds on the stack when
    /// it runs, and so every index of a local, is not checked.
    pub(crate) fn validate(&self) -> Result<(), SteelErr> {
        fn malformed() -> SteelErr {
            SteelErr::new(
                crate::rerrs::ErrorKind::Generic,
                "a function was sent whose body is malformed".to_string(),
            )
        }

        let body = &self.body_exp;
        if body.is_empty() || self.arity > u16::MAX as usize {
            return Err(malformed());
        }

        // The functions being made around the instruction at hand, from the
        // outside in: where the body starts and ends, and how many values
        // it closes over.
        let mut scopes = vec![(0, body.len(), self.captures.len())];
        let mut idx = 0;

        while idx < body.len() {
            let instr = body[idx];
            let (start, end, captures) = *scopes.last().ok_or_else(malformed)?;
            let payload = instr.payload_size.to_usize();

            match instr.op_code {
                OpCode::ECLOSURE => {
                    if scopes.len() == 1 || idx != end {
                        return Err(malformed());
                    }
                    scopes.pop();
                }
                // Left in place of the first instruction by the compiler to
                // native code. A function is sent as it was before that.
                OpCode::DynSuperInstruction => return Err(malformed()),
                _ => {}
            }

            match Operand::of(instr.op_code) {
                Operand::Jump if payload >= end - start => return Err(malformed()),
                Operand::Captured if payload >= captures => return Err(malformed()),
                Operand::Constant if !self.constants.contains_key(&payload) => {
                    return Err(malformed())
                }
                // A call is followed by how many arguments it passes.
                Operand::Global
                    if !matches!(instr.op_code, OpCode::PUSH | OpCode::SET | OpCode::BIND)
                        && idx + 1 >= end =>
                {
                    return Err(malformed())
                }
                Operand::Closure => {
                    // Whether it takes a rest argument, its id, and for one
                    // that closes over something how many values and where
                    // each is found. The body follows, and ends where the
                    // number carried says.
                    let last = idx.checked_add(payload).filter(|last| *last < end);
                    let Some(last) = last else {
                        return Err(malformed());
                    };
                    let mut body_start = idx + 3;
                    let mut closes_over = 0;
                    if instr.op_code == OpCode::NEWSCLOSURE {
                        closes_over = body
                            .get(idx + 3)
                            .filter(|_| idx + 3 < last)
                            .ok_or_else(malformed)?
                            .payload_size
                            .to_usize();
                        body_start = (idx + 4)
                            .checked_add(closes_over)
                            .filter(|start| *start <= last)
                            .ok_or_else(malformed)?;
                        for capture in &body[idx + 4..body_start] {
                            let from_enclosing = matches!(
                                capture.op_code,
                                OpCode::COPYCAPTURECLOSURE
                                    | OpCode::COPYHEAPCAPTURECLOSURE
                                    | OpCode::FIRSTCOPYHEAPCAPTURECLOSURE
                            );
                            if from_enclosing && capture.payload_size.to_usize() >= captures {
                                return Err(malformed());
                            }
                        }
                    }
                    if body_start >= last || body[last].op_code != OpCode::ECLOSURE {
                        return Err(malformed());
                    }
                    scopes.push((body_start, last, closes_over));
                    idx = body_start;
                    continue;
                }
                _ => {}
            }

            idx += 1;
        }

        if scopes.len() != 1 {
            return Err(malformed());
        }
        Ok(())
    }
}

// The instruction slice a frame is currently executing.
//
// Under rooted-instructions this is a raw pointer and a length, and the caller
// has to keep the buffer alive. The jit builds one of these in generated code and
// stores it into VmCore and every StackFrame. Rust doesn't define the order of the
// halves of a *const [T], so the fields are spelled out and the struct is repr(C).
// jit2 needs this variant.
//
// Otherwise it's a counted InstructionPointer, which keeps the buffer alive on its
// own but pays refcount traffic on every frame push.
#[cfg(feature = "rooted-instructions")]
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct RootedInstructions {
    pub(crate) ptr: *const DenseInstruction,
    pub(crate) len: usize,
}

#[cfg(not(feature = "rooted-instructions"))]
#[derive(Clone, PartialEq, Eq)]
pub struct RootedInstructions {
    pub(crate) inner: InstructionPointer<[DenseInstruction]>,
}

// TODO: Come back to this
unsafe impl Send for RootedInstructions {}
unsafe impl Sync for RootedInstructions {}

impl RootedInstructions {
    pub fn new(instructions: InstructionPointer<[DenseInstruction]>) -> Self {
        #[cfg(feature = "rooted-instructions")]
        return Self::from_slice(&instructions);

        #[cfg(not(feature = "rooted-instructions"))]
        return Self {
            inner: instructions,
        };
    }

    // Borrows the slice, so the buffer has to outlive the RootedInstructions
    #[cfg(feature = "rooted-instructions")]
    pub(crate) fn from_slice(slice: &[DenseInstruction]) -> Self {
        Self {
            ptr: slice.as_ptr(),
            len: slice.len(),
        }
    }
}

impl core::fmt::Debug for RootedInstructions {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", &**self)
    }
}

impl core::ops::Deref for RootedInstructions {
    type Target = [DenseInstruction];

    fn deref(&self) -> &Self::Target {
        #[cfg(feature = "rooted-instructions")]
        return unsafe { core::slice::from_raw_parts(self.ptr, self.len) };

        #[cfg(not(feature = "rooted-instructions"))]
        return &self.inner;
    }
}

impl ByteCodeLambda {
    /// The compiled entry point for this function, if it has one.
    #[cfg(feature = "jit2")]
    #[inline]
    pub(crate) fn super_instructions(&self) -> Option<JitFnPointer> {
        self.super_instructions.load()
    }

    /// Install a compiled body.
    #[cfg(feature = "jit2")]
    #[inline]
    pub(crate) fn set_super_instructions(&mut self, pointer: Option<JitFnPointer>) {
        self.super_instructions.store(pointer);
    }

    /// Install a compiled body into a lambda that is already shared, and may be
    /// running on this thread or another. See `JitEntry`.
    #[cfg(feature = "jit2")]
    #[inline]
    pub(crate) fn install_super_instructions(&self, pointer: JitFnPointer) {
        self.super_instructions.store(Some(pointer));
    }

    pub fn new(
        id: u32,
        body_exp: InstructionPointer<[DenseInstruction]>,
        arity: usize,
        is_multi_arity: bool,
        captures: CaptureVec,
    ) -> ByteCodeLambda {
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
            super_instructions: JitEntry::new(None),

            header: None,

            #[cfg(feature = "jit2")]
            tier2: None,
        }
    }

    pub fn from_prototype(proto: &Self, captures: CaptureVec) -> Self {
        Self {
            id: proto.id,
            arity: proto.arity,
            is_multi_arity: proto.is_multi_arity,
            header: proto.header,
            body_exp: proto.body_exp.clone(),
            captures,
            contract: proto.contract.clone(),
            #[cfg(feature = "jit2")]
            super_instructions: {
                // A background compile may have switched the shared body over
                // since the prototype was made. Pick up its entry here once
                // instead of on every instance's first call.
                if proto.super_instructions.load().is_none()
                    && crate::steel_vm::vm::jit::body_entered_natively(&proto.body_exp)
                {
                    crate::steel_vm::vm::jit::resolve_background_entry(proto);
                }
                proto.super_instructions.clone()
            },
            #[cfg(feature = "jit2")]
            tier2: proto.tier2,
        }
    }

    /// Makes the function `value` describes. What it captures and its
    /// constants have been made already.
    pub(crate) fn from_serialized(
        heap: &mut HeapSerializer,
        value: &SerializedLambda,
    ) -> Result<Self, SteelErr> {
        value.validate()?;

        let id = heap.function_id_here(value.id);

        let mut new_body = Vec::with_capacity(value.body_exp.len());
        // Where the ids of the functions made inside this one go.
        let mut closure_ids = Vec::new();
        // Whether the instruction before was a call that has been given
        // back its arity check, which this one then is part of.
        let mut checked = false;

        for (idx, instr) in value.body_exp.iter().enumerate() {
            let mut instr = *instr;

            if core::mem::take(&mut checked) {
                instr.op_code = match instr.op_code {
                    OpCode::FUNCNOARITY => OpCode::FUNC,
                    OpCode::TAILCALLNOARITY => OpCode::TAILCALL,
                    op => op,
                };
            }

            match Operand::of(instr.op_code) {
                Operand::Global => {
                    let global = heap.global_here(instr.payload_size.to_usize())?;
                    if global.index >= 1 << 24 {
                        stop!(Generic => "there are too many globals here to add what a function refers to");
                    }
                    instr.payload_size = u24::from_usize(global.index);

                    // The sender compiled these calls against a function it
                    // knew the arity of. If the one here is not known to be
                    // that function, the call has to check.
                    if !global.as_sent {
                        instr.op_code = match instr.op_code {
                            OpCode::CALLGLOBALNOARITY => OpCode::CALLGLOBAL,
                            OpCode::CALLGLOBALTAILNOARITY => OpCode::CALLGLOBALTAIL,
                            op => op,
                        };
                        checked = true;
                    }
                }

                Operand::Constant => {
                    let old_index = instr.payload_size.to_usize();
                    let Some(constant) = value.constants.get(&old_index) else {
                        stop!(Generic => "a function was sent without a constant it refers to");
                    };
                    let deserialized_constant = heap.built(*constant)?;
                    // Constants are looked up by what they hold, and a cell
                    // can hold itself.
                    if !is_literal(&deserialized_constant, 0) {
                        stop!(Generic => "a function was sent with a constant that is not a literal");
                    }
                    let new_index = heap
                        .thread
                        .compiler
                        .write()
                        .constant_map
                        .add_or_get(deserialized_constant);
                    if new_index >= 1 << 24 {
                        stop!(Generic => "there are too many constants here to add what a function refers to");
                    }
                    instr.payload_size = u24::from_usize(new_index);
                }

                // The function this makes is found by its id once it has
                // been made, so it is given one that nothing here has.
                Operand::Closure => {
                    let closure_id = value.body_exp[idx + 2].payload_size.to_u32();
                    let here = heap.function_id_here(closure_id);
                    if here >= 1 << 24 {
                        stop!(Generic => "there are too many functions here to add another");
                    }
                    closure_ids.push((idx + 2, here));
                }

                Operand::Jump | Operand::Captured | Operand::Other => {}
            }

            new_body.push(instr);
        }

        for (idx, id) in closure_ids {
            new_body[idx].payload_size = u24::from_u32(id);
        }

        // What the body starts with, for when this is compiled and then
        // serialized again.
        let header = new_body.first().map(|instr| instr.op_code);

        let mut made = ByteCodeLambda::new(
            id,
            new_body.into(),
            value.arity,
            value.is_multi_arity,
            value
                .captures
                .iter()
                .map(|capture| heap.built(*capture))
                .collect::<Result<_, _>>()?,
        );
        made.header = header;

        Ok(made)
    }

    pub fn rooted(instructions: InstructionPointer<[DenseInstruction]>) -> ByteCodeLambda {
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

    pub fn set_captures(&mut self, captures: CaptureVec) {
        self.captures = captures;
    }

    // TODO: The lifecycle of `RootedInstructions` should not be
    // beyond the scope of execution. This invariant should in
    // general hold - with the exception of continuations, which
    // should probably hold on to any functions that are contains
    // strongly - so there should be some kind of slot on the continuation
    // to hold on to a strong reference to each instruction set.
    pub(crate) fn body_exp(&self) -> RootedInstructions {
        #[cfg(feature = "rooted-instructions")]
        return RootedInstructions::from_slice(&self.body_exp);

        #[cfg(not(feature = "rooted-instructions"))]
        return RootedInstructions {
            inner: InstructionPointer::clone(&self.body_exp),
        };
    }

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

    pub fn captures(&self) -> &[SteelVal] {
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

    pub fn attach_contract_information(&self, steel_struct: StructRef) {
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
    /// What the function takes and returns, when known from the Rust types of
    /// a function registered with `register_fn`.
    pub signature: Option<HostSignature>,
}

/// The argument and return types of a function registered with
/// `register_fn`, from its Rust signature (see
/// [`crate::rvals::FromSteelVal::ARG_TYPE`] and
/// [`crate::rvals::IntoSteelVal::RETURN_TYPE`]).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct HostSignature {
    pub arg_types: crate::value_type::ArgTypes,
    pub return_type: crate::value_type::ValueType,
    pub raises: crate::value_type::Raises,
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
            signature: None,
        }
    }

    /// Like [`Self::new`], with the signature `register_fn` read off the Rust
    /// types.
    pub(crate) fn new_typed(
        function: Arc<
            dyn Fn(&[SteelVal]) -> crate::rvals::Result<SteelVal> + Send + Sync + 'static,
        >,
        name: Option<&str>,
        arity: Option<u32>,
        signature: HostSignature,
    ) -> Self {
        BoxedDynFunction {
            signature: Some(signature),
            ..Self::new(function, name, arity)
        }
    }

    /// Attaches the signature `register_fn` read off the Rust types.
    pub(crate) fn with_signature(self, signature: HostSignature) -> Self {
        BoxedDynFunction {
            signature: Some(signature),
            ..self
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
            signature: None,
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

// Whether a value is one a compiler puts in the constants: immutable all the
// way down, so nothing in it leads back to itself.
fn is_literal(value: &SteelVal, depth: usize) -> bool {
    if depth > 512 {
        return false;
    }
    match value {
        SteelVal::HeapAllocated(_) | SteelVal::MutableVector(_) | SteelVal::MutableStruct(_) => {
            false
        }
        SteelVal::ListV(list) => list.iter().all(|x| is_literal(x, depth + 1)),
        SteelVal::VectorV(vector) => vector.iter().all(|x| is_literal(x, depth + 1)),
        SteelVal::Pair(pair) => {
            is_literal(pair.car_ref(), depth + 1) && is_literal(pair.cdr_ref(), depth + 1)
        }
        SteelVal::HashMapV(map) => map
            .iter()
            .all(|(k, v)| is_literal(k, depth + 1) && is_literal(v, depth + 1)),
        SteelVal::HashSetV(set) => set.iter().all(|x| is_literal(x, depth + 1)),
        SteelVal::CustomStruct(s) => s.fields().iter().all(|x| is_literal(x, depth + 1)),
        _ => true,
    }
}
