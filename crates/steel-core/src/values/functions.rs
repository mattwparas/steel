#![allow(unused)]

use crate::collections::MutableHashMap as HashMap;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::cell::{Cell, RefCell};
use core::convert::TryFrom;
use core::hash::Hasher;

use fxhash::FxHashSet;

use crate::{
    core::{instructions::DenseInstruction, opcode::OpCode},
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
        vm::{BlockMetadata, BlockPattern, BuiltInSignature},
    },
    // values::contracts::ContractedFunction,
    SteelErr,
    SteelVal,
};

use super::{
    closed::{Heap, HeapRef},
    structs::UserDefinedStruct,
};

// pub(crate) enum Function {
//     BoxedFunction(BoxedFunctionSignature),
//     Closure(Gc<ByteCodeLambda>),
//     FuncV(FunctionSignature),
//     ContractedFunction(Gc<ContractedFunction>),
//     MutFuncV(MutFunctionSignature),
//     Builtin(BuiltInSignature),
// }

// Keep track of this metadata table for getting the docs associated
// with a given function?
pub struct LambdaMetadataTable {
    fn_ptr_table: HashMap<usize, SteelString>,
}

impl Custom for LambdaMetadataTable {}

impl LambdaMetadataTable {
    pub fn new() -> Self {
        Self {
            fn_ptr_table: HashMap::default(),
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
            SteelVal::Closure(b) => {
                let key = b.id as usize;
                self.fn_ptr_table.get(&key).cloned()
            }
            SteelVal::BoxedFunction(b) => {
                let key = Gc::as_ptr(&b) as usize;
                self.fn_ptr_table.get(&key).cloned()
            }
            _ => None,
        }
    }

    // TODO: This will need to get called in other places
    pub fn collect_garbage(&mut self, keep_set: impl Iterator<Item = usize>) {
        let set = keep_set.collect::<crate::collections::MutableHashSet<_>>();

        self.fn_ptr_table.retain(|k, _| set.contains(k));
    }
}

#[cfg(feature = "inline-captures")]
const INLINE_CAPTURE_SIZE: usize = 3;

#[cfg(not(feature = "inline-captures"))]
pub type CaptureVec = Vec<SteelVal>;

#[cfg(feature = "inline-captures")]
pub type CaptureVec = smallvec::SmallVec<[SteelVal; INLINE_CAPTURE_SIZE]>;

#[derive(Clone, Debug)]
pub struct ByteCodeLambda {
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
    pub(crate) captures: CaptureVec,

    // pub(crate) captures: Box<[SteelVal]>
    #[cfg(feature = "dynamic")]
    pub(crate) blocks: RefCell<Vec<(BlockPattern, BlockMetadata)>>,

    // This is a little suspicious, but it should give us the necessary information to attach a struct of metadata
    #[cfg(feature = "sync")]
    contract: SharedMut<Option<Gc<UserDefinedStruct>>>,

    #[cfg(not(feature = "sync"))]
    contract: MutContainer<Option<Gc<UserDefinedStruct>>>,
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
pub struct SerializedLambda {
    pub id: u32,
    pub body_exp: Vec<DenseInstruction>,
    pub arity: usize,
    pub is_multi_arity: bool,
    // TODO: Go ahead and create a ThreadSafeSteelVal where we will just deep clone everything, move
    // it across the thread, and reconstruct on the other side.
    pub captures: Vec<SerializableSteelVal>,
}

#[derive(Clone)]
pub struct SerializedLambdaPrototype {
    pub id: u32,
    pub body_exp: Vec<DenseInstruction>,
    pub arity: usize,
    pub is_multi_arity: bool,
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

impl ByteCodeLambda {
    pub fn new(
        id: u32,
        body_exp: StandardShared<[DenseInstruction]>,
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
        }
    }

    pub(crate) fn from_serialized(heap: &mut HeapSerializer, value: SerializedLambda) -> Self {
        ByteCodeLambda::new(
            value.id,
            value.body_exp.into(),
            value.arity,
            value.is_multi_arity,
            value
                .captures
                .into_iter()
                .map(|x| from_serializable_value(heap, x))
                .collect(),
        )
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
