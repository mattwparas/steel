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
        .register_native_fn_definition(TRANSDUCERS_FUNC_DEFINITION)
        .register_native_fn_definition(REDUCER_TO_INT_DEFINITION)
        .register_native_fn_definition(TRANSDUCER_STAGES_DEFINITION)
        .register_native_fn_definition(REDUCER_PARTS_DEFINITION)
        .register_native_fn_definition(TRANSDUCE_ERROR_DEFINITION)
        .register_native_fn_definition(TRANSDUCE_PLAN_DEFINITION);

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

    // Optimized variants that understand
    // the input data coming in
    MapPair,
}

#[steel_derive::function(name = "#%reducer->int", args(Any), raises = Never)]
pub fn reducer_to_int(arg: &SteelVal) -> Option<SteelVal> {
    if let SteelVal::ReducerV(r) = arg {
        Some(match r.as_ref() {
            crate::values::transducers::Reducer::Sum => SteelVal::IntV(0),
            crate::values::transducers::Reducer::Multiply => SteelVal::IntV(1),
            crate::values::transducers::Reducer::Max => SteelVal::IntV(2),
            crate::values::transducers::Reducer::Min => SteelVal::IntV(3),
            crate::values::transducers::Reducer::Count => SteelVal::IntV(4),
            crate::values::transducers::Reducer::Nth(n) => SteelVal::Pair(Gc::new(
                crate::values::lists::Pair::cons(SteelVal::IntV(5), SteelVal::IntV(*n as _)),
            )),
            crate::values::transducers::Reducer::List => SteelVal::IntV(6),
            crate::values::transducers::Reducer::Vector => SteelVal::IntV(7),
            crate::values::transducers::Reducer::HashMap => SteelVal::IntV(8),
            crate::values::transducers::Reducer::HashSet => SteelVal::IntV(9),
            crate::values::transducers::Reducer::String => SteelVal::IntV(10),
            crate::values::transducers::Reducer::Last => SteelVal::IntV(11),
            crate::values::transducers::Reducer::ForEach(steel_val) => SteelVal::Pair(Gc::new(
                crate::values::lists::Pair::cons(SteelVal::IntV(12), steel_val.clone()),
            )),
            crate::values::transducers::Reducer::Generic(reducer_func) => SteelVal::ListV(
                vec![
                    SteelVal::IntV(13),
                    reducer_func.initial_value.clone(),
                    reducer_func.function.clone(),
                ]
                .into(),
            ),
        })
    } else {
        None
    }
}

#[steel_derive::function(name = "#%transducers->funcs", args(Any), raises = Never)]
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

                        Transducers::MapPair(p) => pair(TransducerKind::MapPair, p.clone()),
                    });
                }
            } else {
                funcs.push(value.clone());
            }
        }

        return Some(SteelVal::ListV(funcs.into()));
    }

    None
}

/// The stages of a transducer in order, each a pair of its kind and what it
/// was made with, or #f for anything that isn't a transducer. The kinds are
/// numbers, since a symbol made here isn't `eq?` to the same symbol in a
/// program: map 0, filter 1, take 2, drop 3, flat-map 4, flatten 5, extend 6,
/// enumerate 7, zip 8, interleave 9, and 10 for the kinds nothing makes.
#[steel_derive::function(name = "#%transducer-stages", args(Any), raises = Never)]
pub fn transducer_stages(value: &SteelVal) -> SteelVal {
    let SteelVal::IterV(transducer) = value else {
        return SteelVal::BoolV(false);
    };
    let mut stages = Vec::new();
    push_stages(transducer, &mut stages);
    SteelVal::ListV(stages.into())
}

fn push_stages(transducer: &Transducer, stages: &mut Vec<SteelVal>) {
    let stage = |kind: isize, argument: &SteelVal| {
        SteelVal::Pair(Gc::new(crate::values::lists::Pair::cons(
            SteelVal::IntV(kind),
            argument.clone(),
        )))
    };
    stages.extend(transducer.ops.iter().map(|op| match op {
        Transducers::Map(f) => stage(0, f),
        Transducers::Filter(f) => stage(1, f),
        Transducers::Take(n) => stage(2, n),
        Transducers::Drop(n) => stage(3, n),
        Transducers::FlatMap(f) => stage(4, f),
        Transducers::Flatten => stage(5, &SteelVal::Void),
        Transducers::Extend(c) => stage(6, c),
        Transducers::Enumerating => stage(7, &SteelVal::Void),
        Transducers::Zipping(c) => stage(8, c),
        Transducers::Interleaving(c) => stage(9, c),
        Transducers::Window(_)
        | Transducers::TakeWhile(_)
        | Transducers::DropWhile(_)
        | Transducers::Cycle
        | Transducers::MapPair(_) => stage(10, &SteelVal::Void),
    }));
}

/// A reducer as a list of its kind and what it was made with, or #f for
/// anything that isn't a reducer. The kinds are numbers, as for
/// `#%transducer-stages`: sum 0, product 1, max 2, min 3, count 4, nth 5,
/// list 6, vector 7, hashmap 8, hashset 9, string 10, last 11, for-each 12,
/// reducer 13.
#[steel_derive::function(name = "#%reducer-parts", args(Any), raises = Never)]
pub fn reducer_parts(value: &SteelVal) -> SteelVal {
    use crate::values::transducers::Reducer;

    let SteelVal::ReducerV(reducer) = value else {
        return SteelVal::BoolV(false);
    };
    let parts = match reducer.as_ref() {
        Reducer::Sum => vec![SteelVal::IntV(0)],
        Reducer::Multiply => vec![SteelVal::IntV(1)],
        Reducer::Max => vec![SteelVal::IntV(2)],
        Reducer::Min => vec![SteelVal::IntV(3)],
        Reducer::Count => vec![SteelVal::IntV(4)],
        Reducer::Nth(n) => vec![SteelVal::IntV(5), SteelVal::IntV(*n as isize)],
        Reducer::List => vec![SteelVal::IntV(6)],
        Reducer::Vector => vec![SteelVal::IntV(7)],
        Reducer::HashMap => vec![SteelVal::IntV(8)],
        Reducer::HashSet => vec![SteelVal::IntV(9)],
        Reducer::String => vec![SteelVal::IntV(10)],
        Reducer::Last => vec![SteelVal::IntV(11)],
        Reducer::ForEach(f) => vec![SteelVal::IntV(12), f.clone()],
        Reducer::Generic(r) => vec![
            SteelVal::IntV(13),
            r.function.clone(),
            r.initial_value.clone(),
        ],
    };
    SteelVal::ListV(parts.into())
}

/// What `transduce` was given, checked as `transduce` checks it: the stages
/// of all the transducers, last first, as `#%transducer-stages` gives them,
/// paired with the reducer's parts, as `#%reducer-parts` gives them. Streams
/// are scheme values, so the second argument says whether the collection is
/// one.
#[steel_derive::native(name = "#%transduce-plan", arity = Exact(2))]
pub fn transduce_plan(args: &[SteelVal]) -> Result<SteelVal> {
    let args_stream = &args[1];
    let SteelVal::ListV(args) = &args[0] else {
        stop!(TypeMismatch => "#%transduce-plan expects a list")
    };
    if args.len() < 2 {
        stop!(ArityMismatch => format!("transduce expects at least 2 arguments, found: {}", args.len()))
    }
    let mut stages = Vec::new();
    let mut transducers = args.iter().skip(1).take(args.len() - 2);
    for value in &mut transducers {
        match value {
            SteelVal::IterV(transducer) => push_stages(transducer, &mut stages),
            _ => stop!(TypeMismatch => format!("transduce expects a transducer, found: {value}")),
        }
    }
    let reducer = args.iter().last().unwrap();
    let parts = reducer_parts(reducer);
    if let SteelVal::BoolV(false) = parts {
        stop!(TypeMismatch => format!("transduce requires that the last argument be a reducer, found: {reducer}"))
    }
    let collection = args.iter().next().unwrap();
    if !matches!(
        collection,
        ListV(_) | VectorV(_) | MutableVector(_) | StringV(_) | HashSetV(_) | HashMapV(_)
    ) && !matches!(args_stream, SteelVal::BoolV(true))
    {
        stop!(TypeMismatch => format!("value unable to be converted to an iterable: {collection}"))
    }
    for stage in &stages {
        let SteelVal::Pair(stage) = stage else {
            continue;
        };
        match (&stage.car, &stage.cdr) {
            (IntV(2), IntV(n)) if *n < 0 => {
                stop!(ContractViolation => format!("taking expects a non-negative integer, found: {n}"))
            }
            (IntV(3), IntV(n)) if *n < 0 => {
                stop!(ContractViolation => format!("dropping expects a non-negative integer, found: {n}"))
            }
            _ => {}
        }
    }
    stages.reverse();
    Ok(SteelVal::Pair(Gc::new(crate::values::lists::Pair::cons(
        SteelVal::ListV(stages.into()),
        parts,
    ))))
}

/// Raises the error `transduce` would: an error of `kind` (`type-mismatch`,
/// `arity-mismatch`, `contract-violation` or `generic`) whose message is
/// `message`, then `value` and `suffix` if given, with `value` shown as the
/// rest of `transduce`'s messages show values.
#[steel_derive::native(name = "#%transduce-error", arity = Range(2, 4))]
pub fn transduce_error(args: &[SteelVal]) -> Result<SteelVal> {
    use crate::rerrs::ErrorKind;

    let kind = match &args[0] {
        SteelVal::SymbolV(s) if s.as_str() == "type-mismatch" => ErrorKind::TypeMismatch,
        SteelVal::SymbolV(s) if s.as_str() == "arity-mismatch" => ErrorKind::ArityMismatch,
        SteelVal::SymbolV(s) if s.as_str() == "contract-violation" => ErrorKind::ContractViolation,
        _ => ErrorKind::Generic,
    };
    let SteelVal::StringV(message) = &args[1] else {
        stop!(TypeMismatch => "#%transduce-error expects a message")
    };
    let mut message = message.to_string();
    if let Some(value) = args.get(2) {
        message.push_str(&value.to_string());
    }
    if let Some(SteelVal::StringV(suffix)) = args.get(3) {
        message.push_str(suffix);
    }
    Err(SteelErr::new(kind, message))
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
#[steel_derive::native(name = "compose", arity = AtLeast(0))]
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
    Ok(core::iter::Cloned<core::slice::Iter<'a, Transducers>>),
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

#[steel_derive::context(name = "transduce", arity = AtLeast(2))]
pub fn transduce(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    if args.len() < 2 {
        builtin_stop!(ArityMismatch => format!("transduce expects at least 2 arguments, found: {}", args.len()); ctx.previous_span())
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
#[steel_derive::function(name = "enumerating", args(), raises = Never)]
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
        // A struct can be a stream
        VectorV(_) | CustomStruct(_) | StringV(_) | ListV(_) | HashSetV(_) | HashMapV(_) => {
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
        // A struct can be a stream
        VectorV(_) | CustomStruct(_) | StringV(_) | ListV(_) | HashSetV(_) | HashMapV(_) => {
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
#[steel_derive::function(name = "mapping", args(Procedure))]
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
        // A struct can be a stream
        VectorV(_) | CustomStruct(_) | StringV(_) | ListV(_) | HashSetV(_) | HashMapV(_) => {
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
#[steel_derive::function(name = "flat-mapping", args(Procedure))]
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
#[steel_derive::function(name = "flattening", args(), raises = Never)]
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
#[steel_derive::function(name = "filtering", args(Procedure))]
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
#[steel_derive::function(name = "taking", args(Int), raises = BadArgs)]
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
#[steel_derive::function(name = "dropping", args(Int), raises = BadArgs)]
pub fn dropping(amt: &SteelVal) -> Result<SteelVal> {
    if let IntV(_) = amt {
        let mut transducer = Transducer::new();
        transducer.push(Transducers::Drop(amt.clone()));
        Ok(SteelVal::IterV(Gc::new(transducer)))
    } else {
        stop!(TypeMismatch => "dropping expects an integer")
    }
}
