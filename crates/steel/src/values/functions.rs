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
    rvals::{BoxedFunctionSignature, FunctionSignature, MutFunctionSignature},
    steel_vm::vm::{BlockMetadata, BlockPattern, BuiltInSignature},
    values::contracts::ContractedFunction,
    SteelVal,
};

use super::closed::HeapRef;

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
    spans: Box<[Span]>,
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
        spans: Box<[Span]>,
    ) -> ByteCodeLambda {
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
            Box::from([]),
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
