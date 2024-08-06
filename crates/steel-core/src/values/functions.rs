#![allow(unused)]

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    convert::TryFrom,
    hash::Hasher,
    ops::Deref,
    rc::Rc,
    sync::Arc,
};

use fxhash::FxHashSet;

use crate::{
    core::{instructions::DenseInstruction, opcode::OpCode},
    gc::Gc,
    parser::{parser::SyntaxObjectId, span::Span},
    rvals::{
        from_serializable_value, into_serializable_value, AsRefSteelVal, BoxedFunctionSignature,
        Custom, FunctionSignature, HeapSerializer, IntoSteelVal, MutFunctionSignature,
        SerializableSteelVal, SteelString,
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

// TODO: replace body exp below with this
// struct Blagh {
//     body: Rc<RefCell<[DenseInstruction]>>,
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
            fn_ptr_table: HashMap::new(),
        }
    }

    pub fn add(&mut self, function: SteelVal, doc: SteelString) {
        match function {
            SteelVal::Closure(b) => {
                self.fn_ptr_table.insert(b.id as _, doc);
            }
            SteelVal::BoxedFunction(b) => {
                self.fn_ptr_table.insert(Rc::as_ptr(&b) as usize, doc);
            }
            _ => {}
        }
    }

    pub fn get(&self, function: SteelVal) -> Option<SteelString> {
        match function {
            SteelVal::Closure(b) => self.fn_ptr_table.get(&(b.id as _)).cloned(),
            SteelVal::BoxedFunction(b) => {
                self.fn_ptr_table.get(&(Rc::as_ptr(&b) as usize)).cloned()
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

#[derive(Clone, Debug)]
pub struct ByteCodeLambda {
    pub(crate) id: u32,
    /// body of the function with identifiers yet to be bound
    #[cfg(feature = "dynamic")]
    pub(crate) body_exp: RefCell<Rc<[DenseInstruction]>>,

    #[cfg(not(feature = "dynamic"))]
    pub(crate) body_exp: Rc<[DenseInstruction]>,

    // #[cfg(not(feature = "dynamic"))]
    // pub(crate) body_exp: Rc<[DenseInstruction]>,
    pub(crate) arity: u16,

    #[cfg(feature = "dynamic")]
    call_count: Cell<usize>,

    pub(crate) is_multi_arity: bool,

    pub(crate) captures: Vec<SteelVal>,
    // TODO: Delete this
    // pub(crate) heap_allocated: RefCell<Vec<HeapRef<SteelVal>>>,
    // pub(crate) spans: Rc<[Span]>,
    #[cfg(feature = "dynamic")]
    pub(crate) blocks: RefCell<Vec<(BlockPattern, BlockMetadata)>>,

    // This is a little suspicious, but it should give us the necessary information to attach a struct of metadata
    contract: RefCell<Option<Gc<UserDefinedStruct>>>,
}

impl PartialEq for ByteCodeLambda {
    fn eq(&self, other: &Self) -> bool {
        // self.body_exp == other.body_exp &&
        self.arity == other.arity && self.id == other.id
    }
}

impl Eq for ByteCodeLambda {}

impl std::hash::Hash for ByteCodeLambda {
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
    // TODO: Go ahead and create a ThreadSafeSteelVal where we will just deep clone everything, move
    // it across the thread, and reconstruct on the other side.
    // pub captures: Vec<SerializableSteelVal>,
}

// #[cfg(feature = "rooted-instructions")]

#[derive(Clone, PartialEq, Eq)]
pub struct RootedInstructions {
    #[cfg(feature = "rooted-instructions")]
    inner: *const [DenseInstruction],
    #[cfg(not(feature = "rooted-instructions"))]
    inner: Rc<[DenseInstruction]>,
}

impl RootedInstructions {
    pub fn new(instructions: Rc<[DenseInstruction]>) -> Self {
        Self {
            #[cfg(feature = "rooted-instructions")]
            inner: Rc::as_ptr(&instructions),
            #[cfg(not(feature = "rooted-instructions"))]
            inner: instructions,
        }
    }
}

impl std::fmt::Debug for RootedInstructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.inner)
    }
}

impl Deref for RootedInstructions {
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

// TODO
// is_let can probably be localized to a specific kind of function
// is_multi_arity can also be localized to a specific kind of function
impl ByteCodeLambda {
    pub fn new(
        id: u32,
        body_exp: Rc<[DenseInstruction]>,
        arity: usize,
        is_multi_arity: bool,
        captures: Vec<SteelVal>,
        // heap_allocated: Vec<HeapRef<SteelVal>>,
    ) -> ByteCodeLambda {
        // debug_assert_eq!(body_exp.len(), spans.len());

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
            // TODO: Allocated the necessary size right away <- we're going to index into it
            // heap_allocated: RefCell::new(heap_allocated),
            // spans,

            // span_id,
            contract: RefCell::new(None),

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
            // Vec::new(),
        )
    }

    pub fn main(instructions: Vec<DenseInstruction>) -> ByteCodeLambda {
        Self::new(
            SyntaxObjectId::fresh().into(),
            instructions.into(),
            0,
            false,
            Vec::default(),
            // Vec::default(),
            // Rc::from([]),
        )
    }

    // pub fn id(&self) -> usize {
    //     self.id
    // }

    pub fn set_captures(&mut self, captures: Vec<SteelVal>) {
        self.captures = captures;
    }

    // pub fn set_heap_allocated(&mut self, heap_allocated: Vec<HeapRef<SteelVal>>) {
    //     self.heap_allocated = RefCell::new(heap_allocated);
    // }

    pub fn body_exp(&self) -> RootedInstructions {
        // #[cfg(feature = "dynamic")]
        // return Rc::clone(&self.body_exp.borrow());

        // #[cfg(not(feature = "dynamic"))]
        // Rc::clone(&self.body_exp)

        #[cfg(not(feature = "rooted-instructions"))]
        return RootedInstructions {
            inner: Rc::clone(&self.body_exp),
        };

        #[cfg(feature = "rooted-instructions")]
        return RootedInstructions {
            inner: Rc::as_ptr(&self.body_exp),
        };
    }

    // Give me just the instruction set, that way we don't need
    // to do a lot of extra cloning just to snag the instructions
    pub unsafe fn raw_body(&self) -> *const [DenseInstruction] {
        return Rc::as_ptr(&self.body_exp);
    }

    pub fn body_mut_exp(&mut self) -> Rc<[DenseInstruction]> {
        #[cfg(feature = "dynamic")]
        return Rc::clone(self.body_exp.get_mut());

        #[cfg(not(feature = "dynamic"))]
        Rc::clone(&self.body_exp)
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
    ) -> (DenseInstruction, Rc<[DenseInstruction]>) {
        let mut guard = self.body_exp.borrow_mut();
        let mut old: Box<[_]> = guard.iter().copied().collect();

        // set up the head instruction to get returned, we'll need it in the block first
        let head_instruction = old[start];

        // Point to the new super instruction
        old[start].op_code = OpCode::DynSuperInstruction;
        old[start].payload_size = super_instruction_id as _;
        *guard = old.into();
        (head_instruction, Rc::clone(&guard))
    }

    #[inline(always)]
    pub fn arity(&self) -> usize {
        self.arity as usize
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
        let mut guard = self.contract.borrow_mut();

        *guard = Some(steel_struct);
    }

    pub fn get_contract_information(&self) -> Option<SteelVal> {
        self.contract
            .borrow()
            .as_ref()
            .map(|x| SteelVal::CustomStruct(x.clone()))
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
    pub name: Option<StaticOrRcStr>,
    pub arity: Option<usize>,
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
        arity: Option<usize>,
    ) -> Self {
        BoxedDynFunction {
            function,
            name: name
                .map(|x| Arc::new(x.to_string()))
                .map(StaticOrRcStr::Owned),
            arity,
        }
    }

    pub(crate) fn new_owned(
        function: Arc<
            dyn Fn(&[SteelVal]) -> crate::rvals::Result<SteelVal> + Send + Sync + 'static,
        >,
        name: Option<Arc<String>>,
        arity: Option<usize>,
    ) -> Self {
        BoxedDynFunction {
            function,
            name: name.map(StaticOrRcStr::Owned),
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
    pub fn get_arity(&self) -> Option<usize> {
        self.arity
    }

    #[inline(always)]
    pub fn name(&self) -> Option<&str> {
        match &self.name {
            Some(StaticOrRcStr::Owned(o)) => None, // TODO: Come back here @Matt
            Some(StaticOrRcStr::Static(s)) => Some(s),
            None => None,
        }
    }
}
