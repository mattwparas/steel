use crate::values::functions;
use crate::{rvals::SteelVal, values::functions::ByteCodeLambda};

// fn can_be_copied_to_thread(function: &ByteCodeLambda) {
//     function
// }


// fn deep_clone(val: SteelVal) {
//     if let SteelVal::BoxedFunction(b) = val {
        
//     }
// }

// Can be naively translated to another thread?
fn is_thread_safe(val: SteelVal) {
    match val {
        SteelVal::Closure(_) => todo!(),
        SteelVal::BoolV(b) => true,
        SteelVal::NumV(_) => todo!(),
        SteelVal::IntV(_) => todo!(),
        SteelVal::CharV(_) => todo!(),
        SteelVal::VectorV(_) => todo!(),
        SteelVal::Void => todo!(),
        SteelVal::StringV(_) => todo!(),
        SteelVal::FuncV(_) => todo!(),
        SteelVal::SymbolV(_) => todo!(),
        SteelVal::Custom(_) => todo!(),
        SteelVal::HashMapV(_) => todo!(),
        SteelVal::HashSetV(_) => todo!(),
        SteelVal::CustomStruct(_) => todo!(),
        SteelVal::PortV(_) => todo!(),
        SteelVal::IterV(_) => todo!(),
        SteelVal::ReducerV(_) => todo!(),
        SteelVal::FutureFunc(_) => todo!(),
        SteelVal::FutureV(_) => todo!(),
        SteelVal::StreamV(_) => todo!(),
        SteelVal::Contract(_) => todo!(),
        SteelVal::ContractedFunction(_) => todo!(),
        SteelVal::BoxedFunction(_) => todo!(),
        SteelVal::ContinuationFunction(_) => todo!(),
        SteelVal::ListV(_) => todo!(),
        SteelVal::MutFunc(_) => todo!(),
        SteelVal::BuiltIn(_) => todo!(),
        SteelVal::MutableVector(_) => todo!(),
        SteelVal::BoxedIterator(_) => todo!(),
        SteelVal::SyntaxObject(_) => todo!(),
        SteelVal::Boxed(_) => todo!(),
        SteelVal::Reference(_) => todo!(),
    }
}