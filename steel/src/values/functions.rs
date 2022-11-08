use std::{
    cell::{Cell, RefCell},
    hash::Hasher,
    rc::{Rc, Weak},
};

use crate::{
    core::instructions::DenseInstruction,
    gc::Gc,
    rvals::{BoxedFunctionSignature, FunctionSignature, MutFunctionSignature},
    steel_vm::vm::BuiltInSignature,
    values::contracts::ContractedFunction,
    SteelVal,
};

use super::closed::HeapRef;

pub(crate) enum Function {
    BoxedFunction(BoxedFunctionSignature),
    Closure(Gc<ByteCodeLambda>),
    FuncV(FunctionSignature),
    ContractedFunction(Gc<ContractedFunction>),
    MutFuncV(MutFunctionSignature),
    Builtin(BuiltInSignature),
}

#[derive(Clone, Debug)]
pub struct ByteCodeLambda {
    /// body of the function with identifiers yet to be bound
    pub(crate) body_exp: Rc<[DenseInstruction]>,
    arity: usize,
    // upvalues: Vec<Weak<RefCell<UpValue>>>,
    call_count: Cell<usize>,
    cant_be_compiled: Cell<bool>,
    pub(crate) is_multi_arity: bool,
    captures: Vec<SteelVal>,
    pub(crate) heap_allocated: RefCell<Vec<HeapRef>>,
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
        body_exp: Vec<DenseInstruction>,
        arity: usize,
        is_multi_arity: bool,
        captures: Vec<SteelVal>,
        heap_allocated: Vec<HeapRef>,
    ) -> ByteCodeLambda {
        ByteCodeLambda {
            body_exp: Rc::from(body_exp.into_boxed_slice()),
            arity,
            call_count: Cell::new(0),
            cant_be_compiled: Cell::new(false),
            is_multi_arity,
            captures,
            // TODO: Allocated the necessary size right away <- we're going to index into it
            heap_allocated: RefCell::new(heap_allocated),
        }
    }

    pub fn set_captures(&mut self, captures: Vec<SteelVal>) {
        self.captures = captures;
    }

    pub fn set_heap_allocated(&mut self, heap_allocated: Vec<HeapRef>) {
        self.heap_allocated = RefCell::new(heap_allocated);
    }

    pub fn body_exp(&self) -> Rc<[DenseInstruction]> {
        Rc::clone(&self.body_exp)
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn heap_allocated(&self) -> &RefCell<Vec<HeapRef>> {
        &self.heap_allocated
    }

    pub fn captures(&self) -> &[SteelVal] {
        &self.captures
    }

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
}
