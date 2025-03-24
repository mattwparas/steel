use crate::gc::shared::ShareableMut;
use crate::gc::Gc;
use crate::primitives::Either;
use crate::rvals::{IntoSteelVal, RestArgsIter, Result, SteelVal};
use crate::rvals::{SteelVal::*, SteelVector};
use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::vm::VmCore;
use crate::values::closed::HeapRef;
use crate::values::lists::Pair;
use crate::values::Vector;
use crate::{stop, throw};

#[steel_derive::define_module(name = "steel/immutable-vectors")]
pub fn immutable_vectors_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/immutable-vectors");

    module
        .register_native_fn_definition(IMMUTABLE_VECTOR_PUSH_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_REST_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_PUSH_FRONT_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_SET_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_DROP_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_TAKE_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_CONSTRUCT_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_CONSTRUCT_ALTERNATE_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_TO_LIST_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_TO_STRING_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_COPY_DEFINITION)
        .register_native_fn_definition(IMMUTABLE_VECTOR_APPEND_DEFINITION)
        .register_native_fn_definition(MAKE_IMMUTABLE_VECTOR_DEFINITION)
        .register_native_fn_definition(VECTOR_COPY_DEFINITION)
        .register_native_fn_definition(VECTOR_APPEND_DEFINITION)
        .register_native_fn_definition(VECTOR_TO_STRING_DEFINITION);

    module
}

#[steel_derive::function(name = "immutable-vector-rest")]
fn immutable_vector_rest(vector: &mut SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.pop_front();
                Ok(std::mem::replace(vector, SteelVal::Void))
            }
            None => Ok(SteelVal::VectorV(SteelVector(Gc::new({
                let mut v = v.unwrap();
                v.pop_front();
                v
            })))),
        },

        _ => {
            stop!(TypeMismatch => "immutable-vector-rest expected either a mutable or immutable vector, found: {:?}", vector);
        }
    }
}

#[steel_derive::function(name = "immutable-vector->list")]
fn immutable_vector_to_list(
    vector: &SteelVector,
    rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    let (start, end) = bounds(rest, "immutable-vector->list", 3, vector)?;

    let items = vector.iter().skip(start).take(end - start).cloned();

    Ok(SteelVal::ListV(items.collect()))
}

#[steel_derive::function(name = "vector->string")]
fn vector_to_string(
    vector: Either<&SteelVector, &HeapRef<Vec<SteelVal>>>,
    rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    match vector {
        Either::Left(vector) => immutable_vector_to_string(vector, rest),
        Either::Right(vec) => {
            let (start, end) = bounds_mut(rest, "vector->string", 3, &vec.get())?;

            vec.get()
                .iter()
                .skip(start)
                .take(end - start)
                .map(|x| {
                    x.char_or_else(throw!(TypeMismatch => "vector->string
                expected a succession of characters"))
                })
                .collect::<Result<String>>()
                .map(|x| x.into())
                .map(SteelVal::StringV)
        }
    }
}

#[steel_derive::function(name = "immutable-vector->string")]
fn immutable_vector_to_string(
    vector: &SteelVector,
    rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    let (start, end) = bounds(rest, "immutable-vector->string", 3, vector)?;

    vector.iter()
        .skip(start)
        .take(end - start)
        .map(|x| {
            x.char_or_else(throw!(TypeMismatch => "immutable-vector->string expected a succession of characters"))
        })
        .collect::<Result<String>>()
        .map(|x| x.into())
        .map(SteelVal::StringV)
}

#[steel_derive::context(name = "vector-copy", arity = "AtLeast(1)")]
fn vector_copy(
    ctx: &mut crate::steel_vm::vm::VmCore,
    args: &[SteelVal],
) -> Option<Result<SteelVal>> {
    fn vector_copy_impl(
        ctx: &mut crate::steel_vm::vm::VmCore,
        args: &[SteelVal],
    ) -> Result<SteelVal> {
        use crate::rvals::FromSteelVal;

        // TODO: Arity check - process the args here
        let mut args_iter = args.iter();

        let vector = args_iter.next();
        let rest = RestArgsIter(args_iter.map(|x| <isize>::from_steelval(x)));

        match vector {
            Some(SteelVal::VectorV(v)) => immutable_vector_copy(v, rest),
            Some(SteelVal::MutableVector(vector)) => {
                let vector = vector.get();
                let (start, end) = bounds_mut(rest, "vector-copy", 3, &vector)?;

                // Have to allocate another thing
                let copy: Vec<_> = vector
                    .iter()
                    .skip(start)
                    .cloned()
                    .take(end - start)
                    .collect();

                let vec = ctx.make_mutable_vector(copy);

                Ok(vec)
            }
            Some(other) => {
                stop!(TypeMismatch => "vector-copy expected a vector, found: {}", other)
            }
            None => todo!(),
        }
    }

    Some(vector_copy_impl(ctx, args))
}

#[steel_derive::function(name = "immutable-vector-copy")]
fn immutable_vector_copy(vector: &SteelVector, rest: RestArgsIter<'_, isize>) -> Result<SteelVal> {
    let (start, end) = bounds(rest, "immutable-vector-copy", 3, vector)?;

    let copy: Vector<_> = vector
        .iter()
        .skip(start)
        .cloned()
        .take(end - start)
        .collect();

    Ok(SteelVal::VectorV(Gc::new(copy).into()))
}

// TODO: Fix this!
#[steel_derive::context(name = "vector-append", arity = "AtLeast(0)")]
fn vector_append(
    ctx: &mut crate::steel_vm::vm::VmCore,
    args: &[SteelVal],
) -> Option<Result<SteelVal>> {
    fn vector_copy_impl(
        ctx: &mut crate::steel_vm::vm::VmCore,
        args: &[SteelVal],
    ) -> Result<SteelVal> {
        // TODO: Preallocate the length by iterating over first
        // and extracting the values?
        let mut vector = Vec::new();

        for arg in args {
            match arg {
                SteelVal::VectorV(v) => {
                    vector.extend(v.iter().cloned());
                }
                SteelVal::MutableVector(v) => {
                    vector.extend(v.get().iter().cloned());
                }
                _ => {
                    stop!(TypeMismatch => "vector-append expects only vectors, found: {}", arg)
                }
            }
        }

        Ok(ctx.make_mutable_vector(vector))
    }

    Some(vector_copy_impl(ctx, args))
}

#[steel_derive::function(name = "immutable-vector-append")]
fn immutable_vector_append(mut rest: RestArgsIter<'_, &SteelVector>) -> Result<SteelVal> {
    let mut vector = Vector::new();

    while let Some(vec) = rest.next().transpose()? {
        vector.extend(vec.iter().cloned());
    }

    Ok(SteelVal::VectorV(Gc::new(vector).into()))
}

#[steel_derive::function(name = "make-immutable-vector")]
fn make_immutable_vector(mut rest: RestArgsIter<'_, &SteelVal>) -> Result<SteelVal> {
    if rest.len() > 2 {
        stop!(ArityMismatch => "make-immutable-vector expects at most 2 arguments");
    }

    let len = match rest.next().transpose()? {
        None => 0,
        Some(SteelVal::IntV(int)) => *int,
        _ => stop!(ContractViolation => "length should be an integer"),
    };

    if len < 0 {
        stop!(ContractViolation => "length should be non-negative");
    }

    let fill = rest.next().transpose()?.unwrap_or(&SteelVal::Void);

    let vector: Vector<_> = std::iter::repeat(())
        .map(|_| fill.clone())
        .take(len as usize)
        .collect();

    Ok(SteelVal::VectorV(Gc::new(vector).into()))
}

/// Pushes a value to the back of the vector, returning a new vector.
#[steel_derive::function(name = "immutable-vector-push")]
fn immutable_vector_push(vector: &mut SteelVal, value: SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.push_back(value);
                Ok(std::mem::replace(vector, SteelVal::Void))
            }
            None => Ok(SteelVal::VectorV(SteelVector(Gc::new({
                let mut v = v.unwrap();
                v.push_back(value);
                v
            })))),
        },

        _ => {
            stop!(TypeMismatch => "immutable-vector-push expected either a mutable or immutable vector, found: {:?}", vector);
        }
    }
}

/// Pushes a value to the back of the vector, returning a new vector.
#[steel_derive::function(name = "vector-push")]
fn vector_push(vector: &mut SteelVal, value: SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.push_back(value);
                Ok(std::mem::replace(vector, SteelVal::Void))
            }
            None => Ok(SteelVal::VectorV(SteelVector(Gc::new({
                let mut v = v.unwrap();
                v.push_back(value);
                v
            })))),
        },

        SteelVal::MutableVector(m) => {
            m.strong_ptr().write().value.push(value);
            Ok(SteelVal::Void)
        }

        _ => {
            stop!(TypeMismatch => "vector-push expected either a mutable or immutable vector, found: {:?}", vector);
        }
    }
}

#[steel_derive::function(name = "vector-push-front")]
fn immutable_vector_push_front(vector: &mut SteelVal, value: SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.push_front(value);
                Ok(std::mem::replace(vector, SteelVal::Void))
            }
            None => Ok(SteelVal::VectorV(SteelVector(Gc::new({
                let mut v = v.unwrap();
                v.push_front(value);
                v
            })))),
        },

        // SteelVal::MutableVector(m) => {
        //     m.strong_ptr().borrow_mut().value.push(value);
        //     Ok(SteelVal::Void)
        // }
        _ => {
            stop!(TypeMismatch => "vector-push-front expected an immutable vector, found: {:?}", vector);
        }
    }
}

#[steel_derive::function(name = "immutable-vector-set")]
fn immutable_vector_set(vector: &mut SteelVal, index: usize, value: SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                if index > v.len() {
                    stop!(Generic => "immutable-vector-set: index out of bounds - attempted to index at offset: {} with length {}", index, v.len());
                }

                v.set(index, value);
                Ok(std::mem::replace(vector, SteelVal::Void))
            }
            None => Ok(SteelVal::VectorV(SteelVector(Gc::new({
                if index > v.len() {
                    stop!(Generic => "immutable-vector-set: index out of bounds - attempted to index at offset: {} with length {}", index, v.len());
                }

                let mut v = v.unwrap();
                v.set(index, value);
                v
            })))),
        },

        _ => {
            stop!(TypeMismatch => "vector-push-front expected an immutable vector, found: {:?}", vector);
        }
    }
}

#[steel_derive::function(name = "immutable-vector-pop-back")]
fn immutable_vector_pop_back(vector: &mut SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                let back = v.pop_back();

                match back {
                    Some(back) => {
                        let vector = std::mem::replace(vector, SteelVal::Void);
                        Ok(SteelVal::Pair(Gc::new(Pair {
                            car: back,
                            cdr: vector,
                        })))
                    }
                    None => Ok(SteelVal::BoolV(false)),
                }
            }
            None => {
                // Ok(SteelVal::VectorV(SteelVector(Gc::new({
                // if index > v.len() {
                //     stop!(Generic => "immutable-vector-set: index out of bounds - attempted to index at offset: {} with length {}", index, v.len());
                // }

                // let mut v = v.unwrap();
                // v.set(index, value);
                // v
                todo!()
                // })))),
            }
        },

        _ => {
            stop!(TypeMismatch => "vector-push-front expected an immutable vector, found: {:?}", vector);
        }
    }
}

#[steel_derive::function(name = "immutable-vector-take")]
fn immutable_vector_take(vector: &mut SteelVal, count: usize) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.truncate(count);
                Ok(std::mem::replace(vector, SteelVal::Void))
            }
            None => Ok(SteelVal::VectorV(SteelVector(Gc::new(v.take(count))))),
        },

        _ => {
            stop!(TypeMismatch => "immutable-vector-take expected an immutable vector, found: {:?}", vector);
        }
    }
}

// #[steel_derive::function(name = "immutable-vector-drop-right")]
// fn immutable_vector_drop_right(vector: &mut SteelVal, count: usize) -> Result<SteelVal> {
//     match vector {
//         SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
//             Some(v) => {
//                 v.truncate(v.len() - count);
//                 Ok(std::mem::replace(vector, SteelVal::Void))
//             }
//             None => Ok(SteelVal::VectorV(SteelVector(Gc::new(
//                 v.take(v.len() - count),
//             )))),
//         },

//         _ => {
//             stop!(TypeMismatch => "immutable-vector-drop-right expected an immutable vector, found: {:?}", vector);
//         }
//     }
// }

#[steel_derive::function(name = "immutable-vector-drop")]
fn immutable_vector_drop(vector: &mut SteelVal, count: usize) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                for _ in 0..count {
                    v.pop_front();
                }

                // v.truncate(count);
                Ok(std::mem::replace(vector, SteelVal::Void))
            }
            None => Ok(SteelVal::VectorV(SteelVector(Gc::new(v.skip(count))))),
        },

        _ => {
            stop!(TypeMismatch => "immutable-vector-drop expected an immutable vector, found: {:?}", vector);
        }
    }
}

// #[steel_derive::function(name = "immutable-vector-take-right")]
// fn immutable_vector_take_right(vector: &mut SteelVal, count: usize) -> Result<SteelVal> {
//     match vector {
//         SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
//             Some(v) => {
//                 for _ in 0..count {
//                     v.pop_front();
//                 }

//                 // v.truncate(count);
//                 Ok(std::mem::replace(vector, SteelVal::Void))
//             }
//             None => Ok(SteelVal::VectorV(SteelVector(Gc::new(v.take(count))))),
//         },

//         _ => {
//             stop!(TypeMismatch => "immutable-vector-drop expected an immutable vector, found: {:?}", vector);
//         }
//     }
// }

#[steel_derive::function(name = "mutable-vector->clear")]
fn mutable_vector_clear(vec: &HeapRef<Vec<SteelVal>>) {
    // Snag the interior value
    vec.strong_ptr().write().value.clear()
}

#[steel_derive::function(name = "mutable-vector->string")]
fn mutable_vector_to_string(vec: &HeapRef<Vec<SteelVal>>) -> Result<SteelVal> {
    let guard = vec.strong_ptr();
    let mut buf = String::new();

    for maybe_char in guard.read().value.iter() {
        if let SteelVal::CharV(c) = maybe_char {
            buf.push(*c);
        } else {
            stop!(TypeMismatch => "mutable-vector->string expected a vector of chars, found: {:?}", maybe_char);
        }
    }

    Ok(buf.into())
}

#[steel_derive::function(name = "mutable-vector-pop!")]
fn mutable_vector_pop(vec: &HeapRef<Vec<SteelVal>>) -> Result<SteelVal> {
    let last = vec.strong_ptr().write().value.pop();

    last.into_steelval()
}

#[steel_derive::context(name = "mutable-vector", arity = "AtLeast(0)")]
pub fn mut_vec_construct(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(Ok(ctx.make_mutable_vector(args.to_vec())))
}

#[steel_derive::context(name = "vector", arity = "AtLeast(0)")]
pub fn mut_vec_construct_vec(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(Ok(ctx.make_mutable_vector(args.to_vec())))
}

#[steel_derive::context(name = "make-vector", arity = "AtLeast(1)")]
pub fn make_vector(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    fn make_vector_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
        match &args {
            &[SteelVal::IntV(i)] if *i >= 0 => {
                Ok(ctx.make_mutable_vector(vec![SteelVal::IntV(0); *i as usize]))
            }
            &[SteelVal::IntV(i), initial_value] if *i >= 0 => {
                Ok(ctx.make_mutable_vector(vec![initial_value.clone(); *i as usize]))
            }
            _ => {
                stop!(TypeMismatch => "make-vector expects a positive integer, and optionally a value to initialize the vector with, found: {:?}", args)
            }
        }
    }
    Some(make_vector_impl(ctx, args))
}

#[steel_derive::function(name = "vector-copy!")]
pub fn mut_vector_copy(
    dest: &HeapRef<Vec<SteelVal>>,
    dest_start: usize,
    src: Either<&HeapRef<Vec<SteelVal>>, &SteelVector>,
    rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    match src {
        Either::Left(src) => {
            if HeapRef::ptr_eq(src, dest) {
                let (src_start, src_end) = {
                    let ptr = src.strong_ptr();
                    let src_guard = &ptr.read().value;
                    bounds_mut(rest, "vector-copy!", 5, src_guard)?
                };

                // Check if the ranges overlap
                let dest_ptr = dest.strong_ptr();
                let dest_guard = &mut dest_ptr.write().value;

                if dest_start > dest_guard.len() {
                    stop!(Generic => "vector-copy!: dest-start must be within the range of the destination vector.
                        Destination vector length: {}, index: {}", dest_guard.len(), dest_start);
                }

                // TODO: Try better to avoid this allocation
                let temporary_buffer = dest_guard[src_start..src_end].to_vec();

                dest_guard
                    .iter_mut()
                    .skip(dest_start)
                    .zip(temporary_buffer.into_iter())
                    .for_each(|(dest, src)| {
                        *dest = src;
                    })
            } else {
                // Source range
                let ptr = src.strong_ptr();
                let src_guard = &ptr.read().value;
                let (src_start, src_end) = bounds_mut(rest, "vector-copy!", 5, src_guard)?;

                let dest_ptr = dest.strong_ptr();
                let dest_guard = &mut dest_ptr.write().value;

                if dest_start > dest_guard.len() {
                    stop!(Generic => "vector-copy!: dest-start must be within the range of the destination vector.
                        Destination vector length: {}, index: {}", dest_guard.len(), dest_start);
                }

                src_guard
                    .iter()
                    .skip(src_start)
                    .take(src_end - src_start)
                    .zip(dest_guard.iter_mut().skip(dest_start))
                    .for_each(|(src, dest)| {
                        *dest = src.clone();
                    });
            }
        }
        Either::Right(src) => {
            // Source range
            let (src_start, src_end) = bounds(rest, "vector-copy!", 5, src)?;

            let dest_ptr = dest.strong_ptr();
            let dest_guard = &mut dest_ptr.write().value;

            if dest_start > dest_guard.len() {
                stop!(Generic => "vector-copy!: dest-start must be within the range of the destination vector.
                    Destination vector length: {}, index: {}", dest_guard.len(), dest_start);
            }

            src.iter()
                .skip(src_start)
                .take(src_end - src_start)
                .zip(dest_guard.iter_mut().skip(dest_start))
                .for_each(|(src, dest)| {
                    *dest = src.clone();
                });
        }
    }

    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "vector-fill!")]
pub fn vector_fill(
    vec: &HeapRef<Vec<SteelVal>>,
    element: SteelVal,
    rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    let ptr = vec.strong_ptr();
    let guard = &mut ptr.write().value;

    let (start, end) = bounds_mut(rest, "vector-fill", 4, guard)?;

    guard
        .iter_mut()
        .skip(start)
        .take(end - start)
        .for_each(|x| *x = element.clone());

    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "mutable-vector->list")]
pub fn mut_vec_to_list(
    vec: &HeapRef<Vec<SteelVal>>,
    rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    let ptr = vec.strong_ptr();
    let guard = &ptr.read().value;

    let (start, end) = bounds_mut(rest, "mutable-vector->list", 3, guard)?;

    let items = guard.iter().skip(start).take(end - start).cloned();

    Ok(SteelVal::ListV(items.collect()))
}

#[steel_derive::function(name = "mut-vec-len")]
pub fn mut_vec_length(vec: &HeapRef<Vec<SteelVal>>) -> SteelVal {
    SteelVal::IntV(vec.get().len() as isize)
}

#[steel_derive::function(name = "vector-set!")]
pub fn mut_vec_set(vec: &HeapRef<Vec<SteelVal>>, i: usize, value: SteelVal) -> Result<SteelVal> {
    let ptr = vec.strong_ptr();

    let guard = &mut ptr.write().value;

    if i as usize > guard.len() {
        stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
    }

    // Update the vector position
    guard[i as usize] = value;

    Ok(SteelVal::Void)
}

#[steel_derive::native(name = "immutable-vector", arity = "AtLeast(0)")]
pub fn immutable_vector_construct(args: &[SteelVal]) -> Result<SteelVal> {
    Ok(SteelVal::VectorV(
        Gc::new(args.iter().cloned().collect::<Vector<_>>()).into(),
    ))
}

// TODO: Fix this naming issue
#[steel_derive::native(name = "vector-immutable", arity = "AtLeast(0)")]
pub fn immutable_vector_construct_alternate(args: &[SteelVal]) -> Result<SteelVal> {
    Ok(SteelVal::VectorV(
        Gc::new(args.iter().cloned().collect::<Vector<_>>()).into(),
    ))
}

#[steel_derive::function(name = "vector-length")]
pub fn vec_length(v: Either<&SteelVector, &HeapRef<Vec<SteelVal>>>) -> SteelVal {
    match v {
        Either::Left(v) => SteelVal::IntV(v.len() as _),
        Either::Right(v) => SteelVal::IntV(v.get().len() as _),
    }
}

#[steel_derive::native(name = "range-vec", constant = true, arity = "Exact(2)")]
pub fn vec_range(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "range takes two arguments");
    }
    let mut args = args.iter();
    match (args.next(), args.next()) {
        (Some(elem), Some(lst)) => {
            if let (IntV(lower), IntV(upper)) = (elem, lst) {
                Ok(SteelVal::VectorV(
                    Gc::new(
                        (*lower as usize..*upper as usize)
                            .into_iter()
                            .map(|x| SteelVal::IntV(x as isize))
                            .collect::<Vector<_>>(),
                    )
                    .into(),
                ))
            } else {
                stop!(TypeMismatch => "range expected number")
            }
        }
        _ => stop!(ArityMismatch => "range takes two arguments"),
    }
}

#[steel_derive::native(name = "mut-vector-ref", constant = true, arity = "Exact(2)")]
pub fn mut_vec_get(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "mut-vector-ref takes two arguments, found: {:?}", args.len())
    }

    let vec = args[0].clone();
    let pos = args[1].clone();

    if let SteelVal::MutableVector(v) = &vec {
        if let SteelVal::IntV(i) = pos {
            if i < 0 {
                stop!(Generic => "mut-vector-ref expects a positive integer, found: {:?}", vec);
            }

            let ptr = v.strong_ptr();

            let guard = &mut ptr.write().value;

            if i as usize >= guard.len() {
                stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
            }

            // Grab the value out of the vector
            Ok(guard[i as usize].clone())
        } else {
            stop!(TypeMismatch => "mut-vector-ref expects an integer, found: {:?}", pos);
        }
    } else {
        stop!(TypeMismatch => "mut-vector-ref expects a vector, found: {:?}", vec);
    }
}

// TODO: This _should_ increase the size count on the maybe_memory_size on the heap
// since it is a growable structure, we'll need to know to rerun the GC when that size
// increases past a certain amount
#[steel_derive::native(name = "vector-push!", constant = true, arity = "Exact(2)")]
pub fn mut_vec_push(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "vector-push! takes two arguments, found: {:?}", args.len())
    }

    let vec = &args[0];

    if let SteelVal::MutableVector(v) = vec {
        // TODO -> make sure this is the correct thing
        // if vec.other_contains_self(&args[1]) {
        //     stop!(Generic => "vector push would create a cyclical reference, which would cause a memory leak")
        // }

        // TODO: disallow cyclical references on construction
        v.strong_ptr().write().value.push(args[1].clone());
        Ok(SteelVal::Void)
    } else {
        stop!(TypeMismatch => "vector-push! expects a vector, found: {:?}", vec);
    }
}

#[steel_derive::native(name = "vector-append!", constant = true, arity = "Exact(2)")]
pub fn mut_vec_append(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "vector-append! takes two arguments, found: {:?}", args.len())
    }

    let vec = args[0].clone();
    let other_vec = args[1].clone();

    if let SteelVal::MutableVector(left) = vec {
        if let SteelVal::MutableVector(right) = other_vec {
            left.strong_ptr()
                .write()
                .value
                .append(&mut right.strong_ptr().write().value);
            Ok(SteelVal::Void)
        } else {
            stop!(TypeMismatch => "vector-append! expects a vector in the second position, found: {:?}", other_vec);
        }
    } else {
        stop!(TypeMismatch => "vector-append! expects a vector in the first position, found: {:?}", vec);
    }
}

pub fn vec_construct_iter<I: Iterator<Item = Result<SteelVal>>>(arg: I) -> Result<SteelVal> {
    let res: Result<Vector<SteelVal>> = arg.collect();
    Ok(SteelVal::VectorV(Gc::new(res?).into()))
}

pub fn vec_construct_iter_normal<I: Iterator<Item = SteelVal>>(arg: I) -> Result<SteelVal> {
    Ok(SteelVal::VectorV(
        Gc::new(arg.collect::<Vector<SteelVal>>()).into(),
    ))
}

pub struct VectorOperations {}
impl VectorOperations {
    pub fn vec_append() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            let lsts: Vector<SteelVal> = unwrap_list_of_lists(args.to_vec())?
                .into_iter()
                .flatten()
                .collect();
            Ok(SteelVal::VectorV(Gc::new(lsts).into()))
        })
    }

    pub fn vec_ref() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "vector-ref takes two arguments");
            }
            let mut args = args.iter();
            match (args.next(), args.next()) {
                (Some(vec), Some(idx)) => {
                    if let SteelVal::MutableVector(v) = &vec {
                        if let SteelVal::IntV(i) = idx.clone() {
                            if i < 0 {
                                stop!(Generic => "vector-ref expects a positive integer, found: {:?}", vec);
                            }

                            let ptr = v.strong_ptr();

                            let guard = &mut ptr.write().value;

                            if i as usize >= guard.len() {
                                stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
                            }

                            // Grab the value out of the vector
                            return Ok(guard[i as usize].clone());
                        } else {
                            stop!(TypeMismatch => "vector-ref expects an integer, found: {:?}", idx);
                        }
                    }

                    if let (VectorV(vec), IntV(idx)) = (vec, idx) {
                        if idx < &0 {
                            stop!(TypeMismatch => "vector-ref expected a positive integer");
                        }

                        let idx: usize = *idx as usize;

                        if idx < vec.len() {
                            Ok(vec[idx].clone())
                        } else {
                            let e = format!("Index out of bounds - attempted to access index: {} with length: {}", idx, vec.len());
                            stop!(Generic => e);
                        }
                    } else {
                        stop!(TypeMismatch => format!("vector-ref expected a vector and a number, found: {vec} and {idx}"))
                    }
                }
                _ => stop!(ArityMismatch => "vector-ref takes two arguments"),
            }
        })
    }

    pub fn vec_push() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "push takes two arguments");
            }
            let mut args = args.iter();
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(l) = lst {
                        let mut l = l.0.unwrap();
                        l.push_back(elem.clone());
                        Ok(SteelVal::VectorV(Gc::new(l).into()))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(elem.clone());
                        new.push_front(lst.clone());
                        Ok(SteelVal::VectorV(Gc::new(new).into()))
                    }
                }
                _ => stop!(ArityMismatch => "push takes two arguments"),
            }
        })
    }

    pub fn vec_cons() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "cons takes two arguments")
            }
            let mut args = args.iter();
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(l) = lst {
                        let mut l = l.0.unwrap();
                        l.push_front(elem.clone());
                        Ok(SteelVal::VectorV(Gc::new(l).into()))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(lst.clone());
                        new.push_front(elem.clone());
                        Ok(SteelVal::VectorV(Gc::new(new).into()))
                    }
                }
                _ => stop!(ArityMismatch => "cons takes two arguments"),
            }
        })
    }

    pub fn vec_car() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "car takes one argument");
            }
            if let Some(first) = args.iter().next() {
                match first {
                    SteelVal::VectorV(e) => {
                        let mut e = e.0.unwrap();
                        match e.pop_front() {
                            Some(e) => Ok(e),
                            None => stop!(ContractViolation => "car expects a non empty list"),
                        }
                    }
                    e => {
                        stop!(TypeMismatch => "car takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "car takes one argument");
            }
        })
    }

    pub fn vec_cdr() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "cdr takes one argument");
            }
            if let Some(first) = args.iter().next() {
                match first {
                    SteelVal::VectorV(e) => {
                        let mut e = e.0.unwrap();
                        if !e.is_empty() {
                            e.pop_front();
                            Ok(SteelVal::VectorV(Gc::new(e).into()))
                        } else {
                            stop!(ContractViolation => "cdr expects a non empty list")
                        }
                    }
                    e => {
                        stop!(TypeMismatch => "cdr takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "cdr takes one argument");
            }
        })
    }

    pub fn list_vec_null() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                match &args[0] {
                    SteelVal::ListV(l) => Ok(l.is_empty().into()),
                    SteelVal::VectorV(v) => Ok(v.is_empty().into()),
                    _ => Ok(SteelVal::BoolV(false)),
                }
            } else {
                stop!(ArityMismatch => "null? takes one argument");
            }
        })
    }
}

fn unwrap_list_of_lists(args: Vec<SteelVal>) -> Result<Vec<Vector<SteelVal>>> {
    args.iter().map(unwrap_single_list).collect()
}

fn unwrap_single_list(exp: &SteelVal) -> Result<Vector<SteelVal>> {
    match exp {
        SteelVal::VectorV(lst) => Ok(lst.0.unwrap()),
        _ => stop!(TypeMismatch => "expected a list"),
    }
}

fn bounds_mut(
    mut rest: RestArgsIter<'_, isize>,
    name: &str,
    args: usize,
    vector: &Vec<SteelVal>,
) -> Result<(usize, usize)> {
    if rest.len() > 2 {
        stop!(ArityMismatch => "{} expects at most {} arguments", name, args);
    }

    let start = rest.next().transpose()?.unwrap_or(0);
    let end = rest.next().transpose()?.unwrap_or(vector.len() as isize);

    if start < 0 || end < 0 {
        stop!(ContractViolation => "start and end must be non-negative");
    }

    let start = start as usize;
    let end = end as usize;

    if end > vector.len() {
        stop!(ContractViolation => "end bound is out of range");
    }

    if start > end {
        stop!(ContractViolation => "start bound cannot be greater than end bound");
    }

    Ok((start, end))
}

fn bounds(
    mut rest: RestArgsIter<'_, isize>,
    name: &str,
    args: usize,
    vector: &Vector<SteelVal>,
) -> Result<(usize, usize)> {
    if rest.len() > 2 {
        stop!(ArityMismatch => "{} expects at most {} arguments", name, args);
    }

    let start = rest.next().transpose()?.unwrap_or(0);
    let end = rest.next().transpose()?.unwrap_or(vector.len() as isize);

    if start < 0 || end < 0 {
        stop!(ContractViolation => "start and end must be non-negative");
    }

    let start = start as usize;
    let end = end as usize;

    if end > vector.len() {
        stop!(ContractViolation => "end bound is out of range");
    }

    if start > end {
        stop!(ContractViolation => "start bound cannot be greater than end bound");
    }

    Ok((start, end))
}

#[cfg(test)]
mod vector_prim_tests {
    use super::*;
    use crate::throw;

    #[cfg(not(feature = "sync"))]
    use im_rc::vector;

    #[cfg(feature = "sync")]
    use im::vector;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<SteelVal> {
        func.func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap()(&args)
    }

    #[test]
    fn vec_construct_test() {
        let args = [SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)];
        let res = immutable_vector_construct(&args);
        let expected = vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)].into();
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_append_test_good_inputs() {
        let args = vec![
            vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)].into(),
            vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)].into(),
            vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)].into(),
        ];

        let res = apply_function(VectorOperations::vec_append(), args);
        let expected = vector![
            SteelVal::IntV(1),
            SteelVal::IntV(2),
            SteelVal::IntV(3),
            SteelVal::IntV(1),
            SteelVal::IntV(2),
            SteelVal::IntV(3),
            SteelVal::IntV(1),
            SteelVal::IntV(2),
            SteelVal::IntV(3)
        ]
        .into();
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_append_test_bad_inputs() {
        let args = vec![
            vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)].into(),
            SteelVal::StringV("foo".into()),
            vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)].into(),
        ];
        let res = apply_function(VectorOperations::vec_append(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_arity_too_few() {
        let args = vec![SteelVal::NumV(1.0)];

        let res = vec_range(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_arity_too_many() {
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(1), SteelVal::IntV(1)];

        let res = vec_range(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_bad_input() {
        let args = vec![SteelVal::StringV("1".into()), SteelVal::NumV(1.0)];

        let res = vec_range(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_normal() {
        let args = vec![SteelVal::IntV(0), SteelVal::IntV(4)];

        let res = vec_range(&args);
        let expected = vector![
            SteelVal::IntV(0),
            SteelVal::IntV(1),
            SteelVal::IntV(2),
            SteelVal::IntV(3)
        ]
        .into();
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_push_arity_too_few() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = apply_function(VectorOperations::vec_push(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_push_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_push_test_good_input_pair() {
        let args = vec![
            SteelVal::StringV("baz".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        let expected = vector![
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into()),
        ]
        .into();
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_push_test_good_input() {
        let args = vec![
            SteelVal::StringV("baz".into()),
            vector![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into())
            ]
            .into(),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        let expected = vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into())
        ]
        .into();
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cons_test_arity_too_few() {
        let args = vec![];
        let res = apply_function(VectorOperations::vec_cons(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cons_test_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cons_pair() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        let expected = vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]
        .into();
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cons_elem_vector() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            vector![
                SteelVal::StringV("bar".into()),
                SteelVal::StringV("baz".into())
            ]
            .into(),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        let expected = vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into())
        ]
        .into();
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_car_arity_too_few() {
        let args = vec![];
        let res = apply_function(VectorOperations::vec_car(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = apply_function(VectorOperations::vec_car(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_bad_input() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = apply_function(VectorOperations::vec_car(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_normal_input() {
        let args = vec![vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]
        .into()];
        let res = apply_function(VectorOperations::vec_car(), args);
        let expected = SteelVal::StringV("foo".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cdr_arity_too_few() {
        let args = vec![];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cdr_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cdr_bad_input() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cdr_normal_input() {
        let args = vec![vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]
        .into()];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        let expected = vector![SteelVal::StringV("bar".into())].into();
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cdr_empty_list() {
        let args = vec![Vector::new().into()];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        assert!(res.is_err());
    }

    #[test]
    fn list_vec_arity() {
        let args = vec![];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        assert!(res.is_err());
    }

    #[test]
    fn list_vec_anything_but_null() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_vec_non_empty_vec() {
        let args = vec![vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]
        .into()];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_vec_empty_vec() {
        let args = vec![Vector::new().into()];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = SteelVal::BoolV(true);
        assert_eq!(res.unwrap(), expected);
    }
}
