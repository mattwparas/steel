#![allow(unused)]

use std::{
    cell::{Cell, RefCell},
    hash::Hasher,
    rc::Rc,
};

use fxhash::FxHashSet;

use crate::{
    core::{instructions::DenseInstruction, opcode::OpCode},
    gc::Gc,
    parser::span::Span,
    rvals::{
        AsRefSteelVal, BoxedFunctionSignature, FunctionSignature, IntoSteelVal,
        MutFunctionSignature,
    },
    steel_vm::vm::{BlockMetadata, BlockPattern, BuiltInSignature},
    values::contracts::ContractedFunction,
    SteelVal,
};

use super::{closed::HeapRef, structs::UserDefinedStruct};

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

#[derive(Clone, Debug)]
pub struct ByteCodeLambda {
    pub(crate) id: usize,
    /// body of the function with identifiers yet to be bound
    pub(crate) body_exp: RefCell<Rc<[DenseInstruction]>>,
    arity: usize,
    call_count: Cell<usize>,
    cant_be_compiled: Cell<bool>,
    pub(crate) is_multi_arity: bool,
    captures: Vec<SteelVal>,
    pub(crate) heap_allocated: RefCell<Vec<HeapRef>>,
    pub(crate) blocks: RefCell<Vec<(BlockPattern, BlockMetadata)>>,
    pub(crate) spans: Rc<[Span]>,

    // This is a little suspicious, but it should give us the necessary information to attach a struct of metadata
    contract: RefCell<Option<Gc<RefCell<UserDefinedStruct>>>>,
}

impl PartialEq for ByteCodeLambda {
    fn eq(&self, other: &Self) -> bool {
        self.body_exp == other.body_exp && self.arity == other.arity
    }
}

impl std::hash::Hash for ByteCodeLambda {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.body_exp.as_ptr().hash(state);
        // self.sub_expression_env.as_ptr().hash(state);
    }
}

// TODO
// is_let can probably be localized to a specific kind of function
// is_multi_arity can also be localized to a specific kind of function
impl ByteCodeLambda {
    pub fn new(
        id: usize,
        body_exp: Vec<DenseInstruction>,
        arity: usize,
        is_multi_arity: bool,
        captures: Vec<SteelVal>,
        heap_allocated: Vec<HeapRef>,
        // TODO: Spans need to be moved around as well, like instructions
        spans: Rc<[Span]>,
    ) -> ByteCodeLambda {
        debug_assert_eq!(body_exp.len(), spans.len());

        ByteCodeLambda {
            id,
            body_exp: RefCell::new(body_exp.into_boxed_slice().into()),
            arity,
            call_count: Cell::new(0),
            cant_be_compiled: Cell::new(false),
            is_multi_arity,
            captures,
            // TODO: Allocated the necessary size right away <- we're going to index into it
            heap_allocated: RefCell::new(heap_allocated),
            blocks: RefCell::new(Vec::new()),
            spans,
            contract: RefCell::new(None),
        }
    }

    pub fn main(instructions: Vec<DenseInstruction>) -> ByteCodeLambda {
        Self::new(
            0,
            instructions,
            0,
            false,
            Vec::default(),
            Vec::default(),
            Rc::from([]),
        )
    }

    pub fn set_captures(&mut self, captures: Vec<SteelVal>) {
        self.captures = captures;
    }

    pub fn set_heap_allocated(&mut self, heap_allocated: Vec<HeapRef>) {
        self.heap_allocated = RefCell::new(heap_allocated);
    }

    pub fn body_exp(&self) -> Rc<[DenseInstruction]> {
        Rc::clone(&self.body_exp.borrow())
    }

    pub fn spans(&self) -> Rc<[Span]> {
        Rc::clone(&self.spans)
    }

    // Get the starting index in the instruction set, and the new ID to associate with this
    // super instruction set.
    // Deep copy the old instruction set, update the new spot to have a dynamic super instruction
    // associated with it.
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
        old[start].payload_size = super_instruction_id as u32;
        *guard = old.into();
        (head_instruction, Rc::clone(&guard))
    }

    #[inline]
    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn heap_allocated(&self) -> &RefCell<Vec<HeapRef>> {
        &self.heap_allocated
    }

    pub fn captures(&self) -> &[SteelVal] {
        &self.captures
    }

    #[inline(always)]
    pub fn increment_call_count(&self) {
        // self.call_count += 1;
        self.call_count.set(self.call_count.get() + 1);
    }

    pub fn call_count(&self) -> usize {
        self.call_count.get()
    }

    pub fn set_cannot_be_compiled(&self) {
        self.cant_be_compiled.set(true)
    }

    pub fn has_attempted_to_be_compiled(&self) -> bool {
        self.cant_be_compiled.get()
    }

    pub fn attach_contract_information(&self, steel_struct: Gc<RefCell<UserDefinedStruct>>) {
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
        stop!(TypeMismatch => "get-contract-struct! expects a function in the first position")
    }
}
