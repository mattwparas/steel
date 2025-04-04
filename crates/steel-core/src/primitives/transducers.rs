use crate::gc::Gc;
use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::stop;

use crate::values::transducers::Transducer;
use crate::values::transducers::Transducers;

// declare_const_ref_functions!(
//     COMPOSE => compose,
//     ENUMERATING => enumerating,
//     ZIPPING => zipping,
//     INTERLEAVING => interleaving,
//     MAPPING => map,
//     EXTENDING => extending,
//     FLAT_MAPPING => flat_map,
//     FLATTENING => flatten,
//     FILTERING => filter,
//     TAKING => take,
//     DROPPING => dropping,
// );

pub fn transducer_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/transducers");

    module
        .register_native_fn_definition(COMPOSE_DEFINITION)
        .register_native_fn_definition(MAPPING_DEFINITION)
        .register_native_fn_definition(FLATTENING_DEFINITION)
        .register_native_fn_definition(FLAT_MAPPING_DEFINITION)
        .register_native_fn_definition(FILTERING_DEFINITION)
        .register_native_fn_definition(TAKING_DEFINITION)
        .register_native_fn_definition(DROPPING_DEFINITION)
        .register_native_fn_definition(EXTENDING_DEFINITION)
        .register_native_fn_definition(ENUMERATING_DEFINITION)
        .register_native_fn_definition(ZIPPING_DEFINITION)
        .register_native_fn_definition(INTERLEAVING_DEFINITION)
        .register_value("into-sum", crate::values::transducers::INTO_SUM)
        .register_value("into-product", crate::values::transducers::INTO_PRODUCT)
        .register_value("into-max", crate::values::transducers::INTO_MAX)
        .register_value("into-min", crate::values::transducers::INTO_MIN)
        .register_value("into-count", crate::values::transducers::INTO_COUNT)
        .register_value("into-list", crate::values::transducers::INTO_LIST)
        .register_value("into-vector", crate::values::transducers::INTO_VECTOR)
        .register_value("into-hashmap", crate::values::transducers::INTO_HASHMAP)
        .register_value("into-hashset", crate::values::transducers::INTO_HASHSET)
        .register_value("into-string", crate::values::transducers::INTO_STRING)
        .register_value("into-last", crate::values::transducers::INTO_LAST)
        .register_value("into-for-each", crate::values::transducers::FOR_EACH)
        .register_value("into-nth", crate::values::transducers::NTH)
        .register_value("into-reducer", crate::values::transducers::REDUCER);
    module
}

#[steel_derive::native(name = "compose", arity = "AtLeast(0)")]
pub fn compose(args: &[SteelVal]) -> Result<SteelVal> {
    let mut transformers = Transducer::new();
    for transducer in args {
        if let IterV(t) = transducer {
            transformers.append(t.unwrap());
        } else {
            stop!(TypeMismatch => "compose only accepts transducers")
        }
    }

    Ok(SteelVal::IterV(Gc::new(transformers)))
}

#[steel_derive::function(name = "enumerating")]
pub fn enumerating() -> Result<SteelVal> {
    let mut transducer = Transducer::new();
    transducer.push(Transducers::Enumerating);
    Ok(SteelVal::IterV(Gc::new(transducer)))
}

#[steel_derive::function(name = "zipping")]
pub fn zipping(iterable: &SteelVal) -> Result<SteelVal> {
    match iterable {
        VectorV(_) | StreamV(_) | StringV(_) | ListV(_) | HashSetV(_) | HashMapV(_) => {
            let mut transducer = Transducer::new();
            transducer.push(Transducers::Zipping(iterable.clone()));
            Ok(SteelVal::IterV(Gc::new(transducer)))
        }
        v => {
            stop!(TypeMismatch => format!("zipping expects an iterable, found: {v:?}"))
        }
    }
}

#[steel_derive::function(name = "interleaving")]
pub fn interleaving(iterable: &SteelVal) -> Result<SteelVal> {
    match &iterable {
        VectorV(_) | StreamV(_) | StringV(_) | ListV(_) | HashSetV(_) | HashMapV(_) => {
            let mut transducer = Transducer::new();
            transducer.push(Transducers::Interleaving(iterable.clone()));
            Ok(SteelVal::IterV(Gc::new(transducer)))
        }
        v => {
            stop!(TypeMismatch => format!("interleaving expects an iterable, found: {v:?}"))
        }
    }
}

#[steel_derive::function(name = "mapping")]
pub fn mapping(func: &SteelVal) -> Result<SteelVal> {
    match &func {
        Closure(_) | FuncV(_) | BoxedFunction(_) | BuiltIn(_) | MutFunc(_) => {
            let mut transducer = Transducer::new();
            transducer.push(Transducers::Map(func.clone()));
            Ok(SteelVal::IterV(Gc::new(transducer)))
        }
        v => stop!(TypeMismatch => format!("mapping expects a function, found: {v:?}")),
    }
}

#[steel_derive::function(name = "extending")]
pub fn extending(iterable: &SteelVal) -> Result<SteelVal> {
    match &iterable {
        VectorV(_) | StreamV(_) | StringV(_) | ListV(_) | HashSetV(_) | HashMapV(_) => {
            let mut transducer = Transducer::new();
            transducer.push(Transducers::Extend(iterable.clone()));
            Ok(SteelVal::IterV(Gc::new(transducer)))
        }
        v => {
            stop!(TypeMismatch => format!("extending expects an iterable, found: {v:?}"))
        }
    }
}

#[steel_derive::function(name = "flat-mapping")]
pub fn flat_mapping(func: &SteelVal) -> Result<SteelVal> {
    match &func {
        Closure(_) | FuncV(_) | BoxedFunction(_) | BuiltIn(_) | MutFunc(_) => {
            let mut transducer = Transducer::new();
            transducer.push(Transducers::FlatMap(func.clone()));
            Ok(SteelVal::IterV(Gc::new(transducer)))
        }
        v => {
            stop!(TypeMismatch => format!("flat-mapping expects a function, found: {v:?}"))
        }
    }
}

#[steel_derive::function(name = "flattening")]
pub fn flattening() -> Result<SteelVal> {
    let mut transducer = Transducer::new();
    transducer.push(Transducers::Flatten);
    Ok(SteelVal::IterV(Gc::new(transducer)))
}

#[steel_derive::function(name = "filtering")]
pub fn filtering(func: &SteelVal) -> Result<SteelVal> {
    match &func {
        Closure(_) | FuncV(_) | BoxedFunction(_) | BuiltIn(_) | MutFunc(_) => {
            let mut transducer = Transducer::new();
            transducer.push(Transducers::Filter(func.clone()));
            Ok(SteelVal::IterV(Gc::new(transducer)))
        }
        _ => stop!(TypeMismatch => "filtering expects a function"),
    }
}

#[steel_derive::function(name = "taking")]
pub fn taking(amt: &SteelVal) -> Result<SteelVal> {
    if let IntV(_) = &amt {
        let mut transducer = Transducer::new();
        transducer.push(Transducers::Take(amt.clone()));
        Ok(SteelVal::IterV(Gc::new(transducer)))
    } else {
        stop!(TypeMismatch => "taking expects an integer")
    }
}

#[steel_derive::function(name = "dropping")]
pub fn dropping(amt: &SteelVal) -> Result<SteelVal> {
    if let IntV(_) = amt {
        let mut transducer = Transducer::new();
        transducer.push(Transducers::Drop(amt.clone()));
        Ok(SteelVal::IterV(Gc::new(transducer)))
    } else {
        stop!(TypeMismatch => "dropping expects an integer")
    }
}
// }
