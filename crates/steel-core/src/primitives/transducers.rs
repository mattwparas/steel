use crate::gc::Gc;
use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::steel_vm::{
    builtin::BuiltInModule,
    vm::{VmContext, VmCore},
};
use crate::values::transducers::{Transducer, Transducers};
use crate::{builtin_stop, stop, SteelErr};

pub fn transducer_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/transducers");

    module
        .register_native_fn_definition(COMPOSE_DEFINITION)
        .register_native_fn_definition(TRANSDUCE_DEFINITION)
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
        .register_value("into-reducer", crate::values::transducers::REDUCER)
        .register_native_fn_definition(TRANSDUCERS_FUNC_DEFINITION);

    module
}

#[repr(usize)]
#[derive(Copy, Clone)]
enum TransducerKind {
    Map,
    Filter,
    Take,
    Drop,
    FlatMap,
    Flatten,
    Window,
    TakeWhile,
    DropWhile,
    Extend,
    Cycle,
    Enumerating,
    Zipping,
    Interleaving,
}

#[steel_derive::function(name = "#%transducers->funcs")]
pub fn transducers_func(arg: &SteelVal) -> Option<SteelVal> {
    let mut funcs = Vec::new();

    fn pair(kind: TransducerKind, cdr: SteelVal) -> SteelVal {
        SteelVal::Pair(Gc::new(crate::values::lists::Pair::cons(
            SteelVal::IntV(kind as usize as _),
            cdr,
        )))
    }

    if let SteelVal::ListV(i) = arg {
        for value in i {
            if let SteelVal::IterV(i) = value {
                for op in &i.ops {
                    funcs.push(match op {
                        Transducers::Map(steel_val) => pair(TransducerKind::Map, steel_val.clone()),
                        Transducers::Filter(steel_val) => {
                            pair(TransducerKind::Filter, steel_val.clone())
                        }
                        Transducers::Take(steel_val) => {
                            pair(TransducerKind::Take, steel_val.clone())
                        }
                        Transducers::Drop(steel_val) => {
                            pair(TransducerKind::Drop, steel_val.clone())
                        }
                        Transducers::FlatMap(steel_val) => {
                            pair(TransducerKind::FlatMap, steel_val.clone())
                        }
                        Transducers::Flatten => pair(TransducerKind::Flatten, SteelVal::Void),
                        Transducers::Window(steel_val) => {
                            pair(TransducerKind::Window, steel_val.clone())
                        }
                        Transducers::TakeWhile(steel_val) => {
                            pair(TransducerKind::TakeWhile, steel_val.clone())
                        }
                        Transducers::DropWhile(steel_val) => {
                            pair(TransducerKind::DropWhile, steel_val.clone())
                        }
                        Transducers::Extend(steel_val) => {
                            pair(TransducerKind::Extend, steel_val.clone())
                        }
                        Transducers::Cycle => pair(TransducerKind::Cycle, SteelVal::Void),
                        Transducers::Enumerating => {
                            pair(TransducerKind::Enumerating, SteelVal::Void)
                        }
                        Transducers::Zipping(steel_val) => {
                            pair(TransducerKind::Zipping, steel_val.clone())
                        }
                        Transducers::Interleaving(steel_val) => {
                            pair(TransducerKind::Interleaving, steel_val.clone())
                        }
                    });
                }
            }
        }
    }

    None
}

/// Compose multiple iterators into one iterator
///
/// (compose . iters) -> iterator?
///
/// # Examples
/// ```scheme
/// (compose
///     (mapping (λ (x) (+ x 1)))
///     (filtering odd?)
///     (taking 15))
/// ```
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

enum FlattenOk<'a> {
    Ok(std::iter::Cloned<std::slice::Iter<'a, Transducers>>),
    Err(Option<SteelErr>),
}

impl Iterator for FlattenOk<'_> {
    type Item = Result<Transducers>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            FlattenOk::Ok(vec) => vec.next().map(Ok),
            FlattenOk::Err(err) => err.take().map(Err),
        }
    }
}

#[steel_derive::context(name = "transduce", arity = "AtLeast(2)")]
pub fn transduce(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    if args.len() < 2 {
        builtin_stop!(ArityMismatch => format!("transduce expects at least 2 arguments, found {}", args.len()); ctx.previous_span())
    }

    let (reducer, args) = args.split_last().expect("already verified length above");

    let mut arg_iter = args.iter();
    let collection = arg_iter.next().unwrap();

    let transducers = match arg_iter
        .flat_map(|x| match x {
            SteelVal::IterV(x) => FlattenOk::Ok(x.ops.iter().cloned()),
            _ => FlattenOk::Err(Some(
                SteelErr::new(
                    crate::rerrs::ErrorKind::TypeMismatch,
                    format!("transduce expects a transducer, found: {x}"),
                )
                .with_span(ctx.previous_span()),
            )),
        })
        .collect::<Result<Vec<_>>>()
    {
        Ok(transducers) => transducers,
        Err(err) => return Some(Err(err)),
    };

    if let SteelVal::ReducerV(reducer) = &reducer {
        if ctx.depth > 32 {
            #[cfg(feature = "stacker")]
            return stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
                Some(ctx.call_transduce(&transducers, collection.clone(), reducer, None))
            });

            #[cfg(not(feature = "stacker"))]
            Some(ctx.call_transduce(&transducers, collection.clone(), reducer, None))
        } else {
            Some(ctx.call_transduce(&transducers, collection.clone(), reducer, None))
        }
    } else {
        builtin_stop!(TypeMismatch => format!("transduce requires that the last argument be a reducer, found: {reducer}"); ctx.previous_span())
    }
}

/// Create an enumerating iterator
///
/// (enumerating) -> iterator?
///
/// # Examples
/// ```scheme
/// (transduce (list 1 3 5) (enumerating) (into-list)) ;; => '((0 1) (1 3) (2 5))
/// ```
#[steel_derive::function(name = "enumerating")]
pub fn enumerating() -> Result<SteelVal> {
    let mut transducer = Transducer::new();
    transducer.push(Transducers::Enumerating);
    Ok(SteelVal::IterV(Gc::new(transducer)))
}

/// Create a zipping iterator
///
/// (zipping any/c) -> iterator?
///
/// # Examples
/// ```scheme
/// (transduce (list 1 2 3) (zipping (list 4 5 6 7)) (into-list)) ;; => '((1 4) (2 5) (3 6))
/// ```
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

/// Create an interleaving iterator
///
/// (interleaving any/c) -> iterator?
///
/// # Examples
/// ```scheme
/// (transduce (list 1 2 3) (interleaving (list 4 5 6)) (into-list)) ;; => '(1 4 2 5 3 6)
/// ```
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

/// Create a mapping iterator
///
/// (mapping proc?) -> iterator?
///
/// # Examples
/// ```scheme
/// (transduce (list 1 2 3) (mapping (λ (x) (+ x 1))) (into-list)) ;; => '(2 3 4)
/// ```
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

/// Create an extending iterator
///
/// (extending iterable) -> iterator?
///
/// # Examples
/// ```scheme
/// (transduce (list 1 2 3) (extending (list 4 5 6 7)) (into-list)) ;; => '(1 2 3 4 5 6 7)
/// ```
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

/// Creates a flat-mapping iterator
///
/// (flat-mapping proc?) -> iterator
///
/// # Examples
/// ```scheme
/// (transduce (list 1 2 3) (flat-mapping (λ (x) (range 0 x))) (into-list)) ;; => '(0 0 1 0 1 2)
/// ```
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

/// Creates a flattening iterator that etc
///
/// (flattening) -> iterator?
///
/// # Examples
/// ```scheme
/// (transduce (list '(1 2) '(3 4) '(5 6)) (flattening) (into-list)) ;; => '(1 2 3 4 5 6)
/// ```
#[steel_derive::function(name = "flattening")]
pub fn flattening() -> Result<SteelVal> {
    let mut transducer = Transducer::new();
    transducer.push(Transducers::Flatten);
    Ok(SteelVal::IterV(Gc::new(transducer)))
}

/// Creates a filtering iterator
///
/// (filtering proc?) -> iterator?
///
/// # Examples
/// ```scheme
/// (transduce (list 1 2 3 4) (filtering even?) (into-list)) ;; => '(2 4)
/// ```
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

/// Creates a taking iterator combinator
///
/// (taking number?) -> iterator?
///
/// # Examples
/// ```scheme
/// (transduce (list 1 2 3 4 5) (taking 3) (into-list)) ;; => '(1 2 3)
/// ```
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

/// Creates a taking iterator combinator
///
/// (dropping integer?) -> iterator?
///
/// # Examples
/// ```scheme
/// (transduce (list 1 2 3 4 5) (dropping 3) (into-list)) ;; => '(4 5)
/// ```
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
