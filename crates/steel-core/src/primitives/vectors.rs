use crate::collections::Vector;
use crate::gc::shared::ShareableMut;
use crate::gc::Gc;
use crate::primitives::Either;
use crate::rvals::SteelVector;
use crate::rvals::{IntoSteelVal, RestArgsIter, Result, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::vm::VmCore;
use crate::values::closed::HeapRef;
use crate::values::lists::Pair;
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

/// Returns the vector with the first value removed.
///
/// (immutable-vector-rest vec) -> immutable-vector?
///
/// * vec : immutable-vector?
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (immutable-vector-rest A) ;; => '#(2 3)
/// ```
#[steel_derive::function(name = "immutable-vector-rest")]
fn immutable_vector_rest(vector: &mut SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.pop_front();
                Ok(core::mem::replace(vector, SteelVal::Void))
            }
            None => Ok(SteelVal::VectorV(SteelVector(Gc::new({
                let mut v = v.unwrap();
                v.pop_front();
                v
            })))),
        },

        _ => {
            stop!(TypeMismatch => "immutable-vector-rest expected either an immutable vector, found: {:?}", vector);
        }
    }
}

/// Converts an immutable vector into a list.
///
/// (immutable-vector->list vec) -> list?
///
/// * vec : immutable-vector?
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (immutable-vector->list A) ;; => '(1 2 3)
/// ```
#[steel_derive::function(name = "immutable-vector->list")]
fn immutable_vector_to_list(
    vector: &SteelVector,
    rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    let (start, end) = bounds(rest, "immutable-vector->list", 3, vector)?;

    let items = vector.iter().skip(start).take(end - start).cloned();

    Ok(SteelVal::ListV(items.collect()))
}

/// Converts a vector of characters into a string.
///
/// (vector->string vec) -> string?
///
/// * vec : vector?
///
/// # Examples
/// ```scheme
/// > (define A (vector #\a #\b #\c)) ;;
/// > (vector->string A) ;; => "abc"
/// ```
#[steel_derive::function(name = "vector->string")]
fn vector_to_string(
    vector: Either<&SteelVector, &HeapRef<Vec<SteelVal>>>,
    rest: RestArgsIter<'_, isize>,
) -> Result<SteelVal> {
    match vector {
        Either::Left(vector) => immutable_vector_to_string(vector, rest),
        Either::Right(vec) => {
            // TODO: Get rid of this unnecessary clone on the get
            let (start, end) = vec.borrow(|x| bounds_mut(rest, "vector->string", 3, x))?;

            vec.borrow(|x| {
                x.iter()
                    .skip(start)
                    .take(end - start)
                    .map(|x| {
                        x.char_or_else(throw!(TypeMismatch => "vector->string
                expected a succession of characters"))
                    })
                    .collect::<Result<String>>()
                    .map(|x| x.into())
                    .map(SteelVal::StringV)
            })
        }
    }
}

/// Converts an immutable vector of characters into a string.
///
/// (immutable-vector->string vec) -> string?
///
/// * vec : immutable-vector?
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector #\a #\b #\c)) ;;
/// > (immutable-vector->string A) ;; => "abc"
/// ```
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

/// Returns a new copy of the given vector.
///
/// (vector-copy vec) -> vector?
///
/// * vec : vector?
///
/// # Examples
/// ```scheme
/// > (define A (vector 1 2 3)) ;;
/// > (vector-copy A) ;; => '#(1 2 3)
/// ```
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
                    .take(end - start)
                    .cloned()
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

/// Returns a new copy of the given immutable vector.
///
/// (immutable-vector-copy vec) -> immutable-vector?
///
/// * vec : immutable-vector?
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (immutable-vector-copy A) ;; => '#(1 2 3)
/// ```
#[steel_derive::function(name = "immutable-vector-copy")]
fn immutable_vector_copy(vector: &SteelVector, rest: RestArgsIter<'_, isize>) -> Result<SteelVal> {
    let (start, end) = bounds(rest, "immutable-vector-copy", 3, vector)?;

    let copy: Vector<_> = vector
        .iter()
        .skip(start)
        .take(end - start)
        .cloned()
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

/// Returns the combination of the given immutable vectors.
///
/// (immutable-vector-append . vecs) -> immutable-vector?
///
/// * vecs : immutable-vector? - The vectors to combine.
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (define B (immutable-vector 4 5 6)) ;;
/// > (immutable-vector-append A B) ;; => '#(1 2 3 4 5 6)
/// ```
#[steel_derive::function(name = "immutable-vector-append")]
fn immutable_vector_append(mut rest: RestArgsIter<'_, &SteelVector>) -> Result<SteelVal> {
    let mut vector = Vector::new();

    while let Some(vec) = rest.next().transpose()? {
        vector.extend(vec.iter().cloned());
    }

    Ok(SteelVal::VectorV(Gc::new(vector).into()))
}

/// Creates an immutable vector of a given length, filled with an optional value.
/// (which defaults to '#<void>')
///
/// (make-immutable-vector len [val]) -> immutable-vector?
///
/// * len : integer?
/// * val : any? - defaults to #<void>
///
/// # Examples
/// ```scheme
/// > (make-immutable-vector 3 5) ;; => '#(5 5 5)
/// > (make-immutable-vector 2) ;; => '#(#<void> #<void>)
/// ```
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

    let vector: Vector<_> = core::iter::repeat(())
        .map(|_| fill.clone())
        .take(len as usize)
        .collect();

    Ok(SteelVal::VectorV(Gc::new(vector).into()))
}

/// Pushes a value to the back of the vector, returning a new vector.
///
/// (immutable-vector-push vec val) -> immutable-vector?
///
/// * vec : immutable-vector?
/// * val : any?
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (immutable-vector-push A 5) ;; => '#(1 2 3 5)
/// ```
#[steel_derive::function(name = "immutable-vector-push")]
fn immutable_vector_push(vector: &mut SteelVal, value: SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.push_back(value);
                Ok(core::mem::replace(vector, SteelVal::Void))
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

// TODO: Register this function?
#[steel_derive::function(name = "vector-push")]
fn vector_push(vector: &mut SteelVal, value: SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.push_back(value);
                Ok(core::mem::replace(vector, SteelVal::Void))
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

/// Pushes a value to the front of the vector, returning a new vector.
///
/// (vector-push-front vec val) -> immutable-vector?
///
/// * vec : immutable-vector?
/// * val : any?
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (vector-push-front A 5) ;; => '#(5 1 2 3)
/// ```
#[steel_derive::function(name = "vector-push-front")]
fn immutable_vector_push_front(vector: &mut SteelVal, value: SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.push_front(value);
                Ok(core::mem::replace(vector, SteelVal::Void))
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

/// Returns a new vector with the specified index updated to the given value.
///
/// (immutable-vector-set vec index val) -> immutable-vector?
///
/// * vec : immutable-vector?
/// * index : integer?
/// * val : any?
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (immutable-vector-set A 0 5) ;; => '#(5 2 3)
/// ```
#[steel_derive::function(name = "immutable-vector-set")]
fn immutable_vector_set(vector: &mut SteelVal, index: usize, value: SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                if index > v.len() {
                    stop!(Generic => "immutable-vector-set: index out of bounds - attempted to index at offset: {} with length {}", index, v.len());
                }

                v.set(index, value);
                Ok(core::mem::replace(vector, SteelVal::Void))
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

// TODO: Register function
#[steel_derive::function(name = "immutable-vector-pop-back")]
fn immutable_vector_pop_back(vector: &mut SteelVal) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                let back = v.pop_back();

                match back {
                    Some(back) => {
                        let vector = core::mem::replace(vector, SteelVal::Void);
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

/// Returns a new vector containing only the first `n` elements of the original vector.
///
/// (immutable-vector-take vec n) -> immutable-vector?
///
/// * vec : immutable-vector?
/// * n : integer?
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3 4)) ;;
/// > (immutable-vector-take A 2) ;; => '#(1 2)
/// ```
#[steel_derive::function(name = "immutable-vector-take")]
fn immutable_vector_take(vector: &mut SteelVal, count: usize) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                v.truncate(count);
                Ok(core::mem::replace(vector, SteelVal::Void))
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
//                 Ok(core::mem::replace(vector, SteelVal::Void))
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

/// Returns a new vector with the first `n` elements removed from the original vector.
///
/// (immutable-vector-drop vec n) -> immutable-vector?
///
/// * vec : immutable-vector?
/// * n : integer?
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3 4)) ;;
/// > (immutable-vector-drop A 2) ;; => '#(3 4)
/// ```
#[steel_derive::function(name = "immutable-vector-drop")]
fn immutable_vector_drop(vector: &mut SteelVal, count: usize) -> Result<SteelVal> {
    match vector {
        SteelVal::VectorV(SteelVector(v)) => match Gc::get_mut(v) {
            Some(v) => {
                for _ in 0..count {
                    v.pop_front();
                }

                // v.truncate(count);
                Ok(core::mem::replace(vector, SteelVal::Void))
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
//                 Ok(core::mem::replace(vector, SteelVal::Void))
//             }
//             None => Ok(SteelVal::VectorV(SteelVector(Gc::new(v.take(count))))),
//         },

//         _ => {
//             stop!(TypeMismatch => "immutable-vector-drop expected an immutable vector, found: {:?}", vector);
//         }
//     }
// }

/// Removes all elements from a mutable vector.
///
/// (mutable-vector->clear vec) -> void?
///
/// * vec : vector?
///
/// # Examples
/// ```scheme
/// > (define A (vector 1 2 3 4)) ;;
/// > (mutable-vector->clear A) ;;
/// > A ;; => '#()
/// ```
#[steel_derive::function(name = "mutable-vector->clear")]
fn mutable_vector_clear(vec: &HeapRef<Vec<SteelVal>>) {
    // Snag the interior value
    vec.strong_ptr().write().value.clear()
}

/// Converts a vector of characters into a string.
///
/// (mutable-vector->string vec) -> string?
///
/// * vec : vector? - (must contain only characters)
///
/// # Examples
/// ```scheme
/// > (define A (vector #\H #\e #\l #\l #\o))
/// > (mutable-vector->string A) ;; => "Hello"
/// ```
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

/// Removes and returns the last element of the vector.
///
/// (mutable-vector-pop! vec) -> any?
///
/// * vec : vector? - the vector to modify
///
/// # Examples
/// ```scheme
/// > (define A (vector 1 2 3))
/// > (mutable-vector-pop! A) ;; => 3
/// > A ;; => '#(1 2)
/// ```
#[steel_derive::function(name = "mutable-vector-pop!")]
fn mutable_vector_pop(vec: &HeapRef<Vec<SteelVal>>) -> Result<SteelVal> {
    let last = vec.strong_ptr().write().value.pop();

    last.into_steelval()
}

/// Constructs a new mutable vector from the provided arguments.
///
/// (mutable-vector . args) -> vector?
///
/// * args : any? - Elements to initialize the mutable vector.
///
/// # Examples
/// ```scheme
/// > (mutable-vector 1 2 3) ;; => '#(1 2 3)
/// ```
#[steel_derive::context(name = "mutable-vector", arity = "AtLeast(0)")]
pub fn mut_vec_construct(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(Ok(ctx.make_mutable_vector(args.to_vec())))
}

/// Constructs a new mutable vector from the provided arguments.
///
/// (vector . args) -> vector?
///
/// * args : any? - Elements to initialize the mutable vector.
///
/// # Examples
/// ```scheme
/// > (vector 1 2 3) ;; => '#(1 2 3)
/// ```
#[steel_derive::context(name = "vector", arity = "AtLeast(0)")]
pub fn mut_vec_construct_vec(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(Ok(ctx.make_mutable_vector(args.to_vec())))
}

/// Creates a mutable vector of a given size, optionally initialized with a specified value.
///
/// (make-vector size [value]) -> vector?
///
/// * size : integer? - The number of elements in the vector (must be non-negative).
/// * value : any? - The value to fill the vector with (defaults to `0` if omitted).
///
/// # Examples
/// ```scheme
/// > (make-vector 3) ;; => '#(0 0 0)
/// > (make-vector 3 42) ;; => '#(42 42 42)
/// ```
#[steel_derive::context(name = "make-vector", arity = "AtLeast(1)")]
pub fn make_vector(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    fn make_vector_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
        match &args {
            &[SteelVal::IntV(i)] if *i >= 0 => Ok(ctx
                .make_mutable_vector_iter(core::iter::repeat(SteelVal::IntV(0)).take(*i as usize))),
            &[SteelVal::IntV(i), initial_value] if *i >= 0 => {
                // Ok(ctx.make_mutable_vector(vec![initial_value.clone(); *i as usize]))
                Ok(ctx.make_mutable_vector_iter(
                    core::iter::repeat(initial_value.clone()).take(*i as usize),
                ))
            }
            _ => {
                stop!(TypeMismatch => "make-vector expects a positive integer, and optionally a value to initialize the vector with, found: {:?}", args)
            }
        }
    }
    Some(make_vector_impl(ctx, args))
}

/// Copies a range of elements from a source vector into a destination mutable vector.
/// Overwrites elements in `dest`, starting at `dest-start`, with elements from `src`
/// within the range `[src-start, src-end)`.
///
/// (vector-copy! dest dest-start src [src-start src-end]) -> void?
///
/// * dest : vector? - The destination mutable vector.
/// * dest-start : integer? - The starting index in the destination vector.
/// * src : vector? - The source vector.
/// * src-start : integer? - The starting index in the source vector (defaults to `0`).
/// * src-end : integer? - The exclusive ending index in the source vector (defaults to the length of `src`).
///
///
/// # Examples
/// ```scheme
/// > (define A (mutable-vector 1 2 3 4 5)) ;;
/// > (define B (mutable-vector 10 20 30 40 50)) ;;
/// > (vector-copy! B 1 A 2 4) ;;
/// > B ;; => '#(10 3 4 40 50)
/// ```
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
                    .zip(temporary_buffer)
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

/// Fills a mutable vector with a specified value over a given range.
///
/// (vector-fill! vec value [start end]) -> void?
///
/// * vec : vector? - The mutable vector to modify.
/// * value : any? - The value to fill the vector with.
/// * start : integer? - The starting index of the fill range (defaults to `0`).
/// * end : integer? - The exclusive ending index of the fill range (defaults to the length of `vec`).
///
/// # Examples
/// ```scheme
/// > (define A (mutable-vector 1 2 3 4 5)) ;;
/// > (vector-fill! A 9 1 4) ;;
/// > A ;; => '#(1 9 9 9 5)
/// ```
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

/// Converts a mutable vector into a list, optionally over a specified range.
///
/// (mutable-vector->list vec [start end]) -> list?
///
/// * vec : vector? - The mutable vector to convert.
/// * start : integer? - The starting index of the range (defaults to `0`).
/// * end : integer? - The exclusive ending index of the range (defaults to the length of `vec`).
///
/// # Examples
/// ```scheme
/// > (define A (mutable-vector 1 2 3 4 5)) ;;
/// > (mutable-vector->list A) ;; => '(1 2 3 4 5)
/// > (mutable-vector->list A 1 4) ;; => '(2 3 4)
/// ```
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

/// Returns the length of a mutable vector.
///
/// (mut-vec-len vec) -> integer?
///
/// * vec : vector? - The mutable vector to retrieve the length of.
///
/// # Examples
/// ```scheme
/// > (define A (mutable-vector 1 2 3 4 5)) ;;
/// > (mut-vec-len A) ;; => 5
/// ```
#[steel_derive::function(name = "mut-vec-len")]
pub fn mut_vec_length(vec: &HeapRef<Vec<SteelVal>>) -> SteelVal {
    SteelVal::IntV(vec.get().len() as isize)
}

/// Sets the value at a specified index in a mutable vector.
///
/// (vector-set! vec index value) -> void?
///
/// * vec : vector? - The mutable vector to modify.
/// * index : integer? - The position in `vec` to update (must be within bounds).
/// * value : any? - The new value to store at `index`.
///
/// # Examples
/// ```scheme
/// > (define A (mutable-vector 1 2 3)) ;;
/// > (vector-set! A 1 42) ;;
/// > A ;; => '#(1 42 3)
/// ```
#[steel_derive::function(name = "vector-set!")]
pub fn mut_vec_set(vec: &HeapRef<Vec<SteelVal>>, i: usize, value: SteelVal) -> Result<SteelVal> {
    let ptr = vec.strong_ptr();

    let guard = &mut ptr.write().value;

    if i >= guard.len() {
        stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
    }

    // Update the vector position
    guard[i] = value;

    Ok(SteelVal::Void)
}

/// Swaps the value of the specified indices in a mutable vector.
///
/// (vector-swap! vec a b) -> void?
///
/// * vec : vector? - The mutable vector to modify.
/// * a : integer? - The first index of `vec` to swap with `b` (must be within bounds).
/// * b : integer? - The first index of `vec` to swap with `a` (must be within bounds).
///
/// # Examples
/// ```scheme
/// > (define A (mutable-vector 1 2 3)) ;;
/// > (vector-swap! A 0 1) ;;
/// > A ;; => '#(2 1 3)
/// ```
#[steel_derive::function(name = "vector-swap!")]
pub fn mut_vec_swap(vec: &HeapRef<Vec<SteelVal>>, i: usize, j: usize) -> Result<SteelVal> {
    let ptr = vec.strong_ptr();

    let guard = &mut ptr.write().value;

    if i >= guard.len() {
        stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
    } else if j >= guard.len() {
        stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", j, guard.len());
    }

    guard.swap(i, j);

    Ok(SteelVal::Void)
}

/// Constructs an immutable vector from the given arguments.
///
/// (immutable-vector . vals) -> vector?
///
/// * vals : any? - The values to store in the immutable vector.
///
/// # Examples
/// ```scheme
/// > (define V (immutable-vector 1 2 3)) ;;
/// > V ;; => '#(1 2 3)
/// ```
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

/// Returns the length of the given vector.
///
/// (vector-length vec) -> integer?
///
/// * vec : vector? - The vector whose length is to be determined.
///
/// # Examples
/// ```scheme
/// > (define V (immutable-vector 1 2 3 4)) ;;
/// > (vector-length V) ;; => 4
/// ```
#[steel_derive::function(name = "vector-length")]
pub fn vec_length(v: Either<&SteelVector, &HeapRef<Vec<SteelVal>>>) -> SteelVal {
    match v {
        Either::Left(v) => SteelVal::IntV(v.len() as _),
        Either::Right(v) => SteelVal::IntV(v.borrow(|x| x.len() as _)),
    }
}

/// Constructs a vector containing a range of integers from `start` to `end` (exclusive).
///
/// (range-vec start end) -> immutable-vector?
///
/// * start : integer? - The starting value of the range (inclusive).
/// * end : integer? - The ending value of the range (exclusive).
///
/// # Examples
/// ```scheme
/// > (range-vec 1 5) ;; => '#(1 2 3 4)
/// ```
#[steel_derive::native(name = "range-vec", constant = true, arity = "Exact(2)")]
pub fn vec_range(args: &[SteelVal]) -> Result<SteelVal> {
    match (&args[0], &args[1]) {
        (SteelVal::IntV(lower), SteelVal::IntV(upper)) => Ok(SteelVal::VectorV(
            Gc::new(
                (*lower as usize..*upper as usize)
                    .map(|x| SteelVal::IntV(x as isize))
                    .collect::<Vector<_>>(),
            )
            .into(),
        )),
        _ => stop!(TypeMismatch => "range expected number"),
    }
}

/// Retrieves the value at a specified index in a mutable vector.
///
/// (mut-vector-ref vec index) -> any?
///
/// * vec : vector? - The mutable vector from which to retrieve a value.
/// * index : integer? - The position in `vec` to access (must be within bounds).
///
/// # Examples
/// ```scheme
/// > (define A (mutable-vector 10 20 30)) ;;
/// > (mut-vector-ref A 1) ;; => 20
/// ```
#[steel_derive::native(name = "mut-vector-ref", constant = true, arity = "Exact(2)")]
pub fn mut_vec_get(args: &[SteelVal]) -> Result<SteelVal> {
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

/// Appends the contents of one mutable vector to another.
///
/// (vector-append! vec1 vec2) -> void?
///
/// * vec1 : vector? - The mutable vector to which elements will be appended.
/// * vec2 : vector? - The mutable vector whose elements will be appended to `vec1`.
///
/// # Examples
/// ```scheme
/// > (define A (mutable-vector 1 2)) ;;
/// > (define B (mutable-vector 3 4)) ;;
/// > (vector-append! A B) ;;
/// > A ;; => '#(1 2 3 4)
/// > B ;; => '#()
/// ```
#[steel_derive::native(name = "vector-append!", constant = true, arity = "Exact(2)")]
pub fn mut_vec_append(args: &[SteelVal]) -> Result<SteelVal> {
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

/// Combines the given vectors.
///
/// (vec-append . vecs) -> immutable-vector?
///
/// * vecs : immutable-vector? - The vectors to combine.
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (define B (immutable-vector 4 5)) ;;
/// > (vec-append A B) ;; => '#(1 2 3 4 5)
/// ```
#[steel_derive::native(name = "vec-append", constant = true, arity = "AtLeast(0)")]
pub fn vec_append(args: &[SteelVal]) -> Result<SteelVal> {
    let lsts: Vector<SteelVal> = unwrap_list_of_lists(args.to_vec())?
        .into_iter()
        .flatten()
        .collect();
    Ok(SteelVal::VectorV(Gc::new(lsts).into()))
}

/// Retrieves the value at a specified index in an immutable or mutable vector.
///
/// (vector-ref vec index) -> any?
///
/// * vec : vector? - The vector from which to retrieve a value.
/// * index : integer? - The position in `vec` to access (must be within bounds).
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 10 20 30)) ;;
/// > (vector-ref A 1) ;; => 20
/// > (define B (mutable-vector 5 15 25)) ;;
/// > (vector-ref B 2) ;; => 25
/// ```
#[steel_derive::function(name = "vector-ref", constant = true)]
pub fn vec_ref(vec: &SteelVal, idx: &SteelVal) -> Result<SteelVal> {
    // First, ensure the index is a valid non-negative integer
    if let SteelVal::IntV(i) = idx {
        if *i < 0 {
            stop!(Generic => "vector-ref expects a positive integer, found: {:?}", i);
        }

        let idx_usize = *i as usize;

        // Now match on the vector type
        match vec {
            SteelVal::MutableVector(v) => {
                // TODO: If we move this into a context aware function,
                // then we can avoid the lookup cost since we won't be in a safepoint.
                let ptr = v.strong_ptr();
                let guard = &ptr.read().value;

                if idx_usize >= guard.len() {
                    stop!(Generic => "index out of bounds, index given: {:?}, length of vector: {:?}", i, guard.len());
                }

                Ok(guard[idx_usize].clone())
            }

            SteelVal::VectorV(v) => {
                if idx_usize < v.len() {
                    Ok(v[idx_usize].clone())
                } else {
                    let e = format!(
                        "Index out of bounds - attempted to access index: {} with length: {}",
                        idx_usize,
                        v.len()
                    );
                    stop!(Generic => e);
                }
            }

            _ => stop!(TypeMismatch => format!(
                "vector-ref expected a vector and a number, found: {:?} and {:?}",
                vec, idx
            )),
        }
    } else {
        stop!(TypeMismatch => "vector-ref expects an integer index, found: {:?}", idx);
    }
}

/// Appends an element to the given vector.
///
/// (push elem vec) -> immutable-vector?
///
/// * elem : any - The element to append.
/// * vec : immutable-vector? - The vector to which the element will be appended.
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (push 4 A) ;; => '#(1 2 3 4)
/// ```
#[steel_derive::native(name = "push", constant = true, arity = "Exact(2)")]
pub fn vec_push(args: &[SteelVal]) -> Result<SteelVal> {
    let elem = &args[0];
    let lst = &args[1];

    match lst {
        SteelVal::VectorV(v) => {
            let mut vec = v.0.unwrap();
            vec.push_back(elem.clone());
            Ok(SteelVal::VectorV(Gc::new(vec).into()))
        }
        _ => {
            let mut new = Vector::new();
            new.push_front(elem.clone());
            new.push_front(lst.clone());
            Ok(SteelVal::VectorV(Gc::new(new).into()))
        }
    }
}

/// Prepends an element to the given vector.
///
/// (push-front elem vec) -> immutable-vector?
///
/// * elem : any - The element to prepend.
/// * vec : immutable-vector? - The vector to which the element will be prepended.
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 2 3 4)) ;;
/// > (push-front 1 A) ;; => '#(1 2 3 4)
/// ```
#[steel_derive::native(name = "push-front", constant = true, arity = "Exact(2)")]
pub fn vec_cons(args: &[SteelVal]) -> Result<SteelVal> {
    let elem = &args[0];
    let lst = &args[1];

    match lst {
        SteelVal::VectorV(v) => {
            let mut vec = v.0.unwrap();
            vec.push_front(elem.clone());
            Ok(SteelVal::VectorV(Gc::new(vec).into()))
        }
        _ => {
            let mut new = Vector::new();
            new.push_front(lst.clone());
            new.push_front(elem.clone());
            Ok(SteelVal::VectorV(Gc::new(new).into()))
        }
    }
}

/// Returns the first element of the given vector.
///
/// (pop-front vec) -> any?
///
/// * vec : immutable-vector? - The vector from which the first element will be returned.
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (pop-front A) ;; => 1
/// ```
#[steel_derive::native(name = "pop-front", constant = true, arity = "Exact(1)")]
pub fn vec_car(args: &[SteelVal]) -> Result<SteelVal> {
    match &args[0] {
        SteelVal::VectorV(v) => {
            let mut vec = v.0.unwrap();
            match vec.pop_front() {
                Some(val) => Ok(val),
                None => stop!(ContractViolation => "car expects a non-empty list"),
            }
        }
        other => stop!(TypeMismatch => "car expects a list, given: {}", other),
    }
}

/// Returns a new vector with the first element removed.
///
/// (vec-rest vec) -> immutable-vector?
///
/// * vec : immutable-vector? - The vector from which the first element will be removed.
///
/// # Examples
/// ```scheme
/// > (define A (immutable-vector 1 2 3)) ;;
/// > (vec-rest A) ;; => '#(2 3)
/// ```
#[steel_derive::native(name = "vec-rest", constant = true, arity = "Exact(1)")]
pub fn vec_cdr(args: &[SteelVal]) -> Result<SteelVal> {
    match &args[0] {
        SteelVal::VectorV(v) => {
            let mut vec = v.0.unwrap();
            if vec.is_empty() {
                stop!(ContractViolation => "cdr expects a non-empty list");
            } else {
                vec.pop_front();
                Ok(SteelVal::VectorV(Gc::new(vec).into()))
            }
        }
        other => stop!(TypeMismatch => "cdr expects a list, given: {}", other),
    }
}

/// Checks if the given list or vector is empty.
///
/// (null? obj) -> boolean?
///
/// * obj : (or/c list? vector?) - The list or vector to check.
///
/// # Examples
/// ```scheme
/// > (null? (vector)) ;; => #t
/// > (null? (immutable-vector 1 2 3)) ;; => #f
/// > (null? '()) ;; => #t
/// > (null? '(1 2 3)) ;; => #f
/// ```
#[steel_derive::native(name = "null?", constant = true, arity = "Exact(1)")]
pub fn list_vec_null(args: &[SteelVal]) -> Result<SteelVal> {
    let result = match &args[0] {
        SteelVal::ListV(l) => l.is_empty(),
        SteelVal::VectorV(v) => v.is_empty(),
        SteelVal::MutableVector(v) => {
            let ptr = v.strong_ptr();
            let guard = &ptr.read().value;
            guard.is_empty()
        }
        _ => false,
    };

    Ok(SteelVal::BoolV(result))
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
    vector: &[SteelVal],
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

    #[cfg(not(feature = "sync"))]
    use im_rc::vector;

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    use im::vector;

    #[cfg(all(feature = "sync", feature = "imbl"))]
    use imbl::vector;

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

        let res = vec_append(&args);
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
        let res = vec_append(&args);
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
        let res = vec_push(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_push_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
        ];
        let res = vec_push(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_push_test_good_input_pair() {
        let args = vec![
            SteelVal::StringV("baz".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = vec_push(&args);
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
        let res = vec_push(&args);
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
        let res = vec_cons(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cons_test_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
        ];
        let res = vec_cons(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cons_pair() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = vec_cons(&args);
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
        let res = vec_cons(&args);
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
        let res = vec_car(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = vec_car(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_bad_input() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = vec_car(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_normal_input() {
        let args = vec![vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]
        .into()];
        let res = vec_car(&args);
        let expected = SteelVal::StringV("foo".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cdr_arity_too_few() {
        let args = vec![];
        let res = vec_cdr(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cdr_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = vec_cdr(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cdr_bad_input() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = vec_cdr(&args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cdr_normal_input() {
        let args = vec![vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]
        .into()];
        let res = vec_cdr(&args);
        let expected = vector![SteelVal::StringV("bar".into())].into();
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cdr_empty_list() {
        let args = vec![Vector::new().into()];
        let res = vec_cdr(&args);
        assert!(res.is_err());
    }

    #[test]
    fn list_vec_arity() {
        let args = vec![];
        let res = list_vec_null(&args);
        assert!(res.is_err());
    }

    #[test]
    fn list_vec_anything_but_null() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = list_vec_null(&args);
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
        let res = list_vec_null(&args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_vec_empty_vec() {
        let args = vec![Vector::new().into()];
        let res = list_vec_null(&args);
        let expected = SteelVal::BoolV(true);
        assert_eq!(res.unwrap(), expected);
    }
}
