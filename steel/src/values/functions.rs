use crate::{
    gc::Gc,
    rvals::{
        BoxedFunctionSignature, ByteCodeLambda, FunctionSignature, MutFunctionSignature,
        SteelVal::*,
    },
    values::contracts::ContractedFunction,
};

pub(crate) enum Function {
    BoxedFunction(BoxedFunctionSignature),
    Closure(Gc<ByteCodeLambda>),
    FuncV(FunctionSignature),
    ContractedFunction(Gc<ContractedFunction>),
    MutFuncV(MutFunctionSignature),
}
