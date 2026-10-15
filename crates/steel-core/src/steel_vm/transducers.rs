use crate::values::closed::HeapVec;
// use im_lists::list::List;
use crate::values::{lists::List, HashSet};
// use itertools::Itertools;

// use super::{evaluation_progress::EvaluationProgress, stack::StackFrame, vm::VmCore};
use super::vm::VmCore;
use crate::{
    gc::Gc,
    parser::span::Span,
    primitives::numbers::{add_primitive, multiply_primitive},
    primitives::vectors::vec_construct_iter,
    rerrs::{ErrorKind, SteelErr},
    rvals::{Result, SteelVal},
    steel_vm::primitives::{greater_than, less_than},
    values::transducers::{Reducer, Transducers},
};

use crate::values::HashMap;
use core::cell::RefCell;
use std::{iter::Fuse, rc::Rc};

/// An iterator adaptor that alternates elements from two iterators until both
/// run out.
///
/// This iterator is *fused*.
///
/// See [`.interleave()`](crate::Itertools::interleave) for more information.
#[derive(Clone, Debug)]
#[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
pub struct Interleave<I, J> {
    a: Fuse<I>,
    b: Fuse<J>,
    flag: bool,
}

/// Create an iterator that interleaves elements in `i` and `j`.
///
/// [`IntoIterator`] enabled version of `[Itertools::interleave]`.
pub fn interleave<I, J>(
    i: I,
    j: J,
) -> Interleave<<I as IntoIterator>::IntoIter, <J as IntoIterator>::IntoIter>
where
    I: IntoIterator,
    J: IntoIterator<Item = I::Item>,
{
    Interleave {
        a: i.into_iter().fuse(),
        b: j.into_iter().fuse(),
        flag: false,
    }
}

impl<I, J> Iterator for Interleave<I, J>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    type Item = I::Item;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.flag = !self.flag;
        if self.flag {
            match self.a.next() {
                None => self.b.next(),
                r => r,
            }
        } else {
            match self.b.next() {
                None => self.a.next(),
                r => r,
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        add(self.a.size_hint(), self.b.size_hint())
    }
}

/// `SizeHint` is the return type of `Iterator::size_hint()`.
pub type SizeHint = (usize, Option<usize>);

/// Add `SizeHint` correctly.
#[inline]
pub fn add(a: SizeHint, b: SizeHint) -> SizeHint {
    let min = a.0.saturating_add(b.0);
    let max = match (a.1, b.1) {
        (Some(x), Some(y)) => x.checked_add(y),
        _ => None,
    };

    (min, max)
}

impl<I, J> core::iter::FusedIterator for Interleave<I, J>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
}

/// Generates the take transducer - wrapper around the take iterator
macro_rules! generate_take {
    ($iter:expr, $num:expr, $cur_inst_span:expr) => {
        if let SteelVal::IntV(num) = $num {
            if *num < 0 {
                stop!(ContractViolation => format!("taking expects a non-negative integer, found: {num}"); *$cur_inst_span)
            }
            Box::new($iter.take(*num as usize))
        } else {
            stop!(TypeMismatch => "take transducer takes an integer"; *$cur_inst_span)
        }
    }
}

/// Generates the drop transducer - wrapper around the drop iterator
macro_rules! generate_drop {
    ($iter:expr, $num:expr, $cur_inst_span:expr) => {
        if let SteelVal::IntV(num) = $num {
            if *num < 0 {
                stop!(ContractViolation => format!("dropping expects a non-negative integer, found: {num}"); *$cur_inst_span)
            }
            Box::new($iter.skip(*num as usize))
        } else {
            stop!(TypeMismatch => "drop transducer takes an integer"; *$cur_inst_span)
        }
    }
}

impl<'global, 'a> VmCore<'a> {
    // With transducers, we also need reducers
    // reducers should define _how_ a value is going to be converted away
    // from the iterator stream
    // This could either be a function that returns a single value, or a generic collection type
    pub(crate) fn res_iterator(
        value: &'global SteelVal,
        // The nursery here is for iterating over a vec since its wrapped inside the refcell
        nursery: &'global mut Option<HeapVec>,
    ) -> Result<Box<dyn Iterator<Item = Result<SteelVal>> + 'global>> {
        match value {
            SteelVal::VectorV(v) => Ok(Box::new(v.iter().cloned().map(Ok))),
            SteelVal::StringV(s) => Ok(Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x))))),
            SteelVal::ListV(l) => Ok(Box::new(l.clone().into_iter().map(Ok))),
            SteelVal::HashSetV(hs) => Ok(Box::new(hs.iter().cloned().map(Ok))),
            SteelVal::HashMapV(hm) => {
                Ok(Box::new(hm.iter().map(|x| {
                    Ok(SteelVal::ListV(vec![x.0.clone(), x.1.clone()].into()))
                })))
            }
            SteelVal::MutableVector(v) => {
                // Copy over the mutable vector into the nursery
                *nursery = Some(v.get());

                Ok(Box::new(nursery.as_ref().unwrap().iter().cloned().map(Ok)))
            }
            _ => {
                stop!(TypeMismatch => format!("value unable to be converted to an iterable: {value}"))
            }
        }
    }

    /// The values of a collection a transducer brings in, in the order
    /// `transduce` would see them. Anything else is an error, which `what`
    /// names.
    fn collection_iterator(
        value: SteelVal,
        cur_inst_span: &'global Span,
        what: &str,
    ) -> Box<dyn Iterator<Item = Result<SteelVal>> + 'global> {
        match value {
            SteelVal::ListV(l) => Box::new(l.into_iter().map(Ok)),
            SteelVal::VectorV(v) => Box::new(v.0.unwrap().into_iter().map(Ok)),
            SteelVal::MutableVector(v) => Box::new(v.get().into_iter().map(Ok)),
            SteelVal::StringV(s) => Box::new(
                s.chars()
                    .map(|x| Ok(SteelVal::CharV(x)))
                    .collect::<Vec<_>>()
                    .into_iter(),
            ),
            SteelVal::HashSetV(hs) => {
                Box::new(hs.iter().cloned().map(Ok).collect::<Vec<_>>().into_iter())
            }
            SteelVal::HashMapV(hm) => Box::new(
                hm.iter()
                    .map(|(k, v)| Ok(SteelVal::ListV(vec![k.clone(), v.clone()].into())))
                    .collect::<Vec<_>>()
                    .into_iter(),
            ),
            other => Box::new(core::iter::once(Err(SteelErr::new(
                ErrorKind::TypeMismatch,
                format!("{what} expected a traversable value, found: {other}"),
            )
            .with_span(*cur_inst_span)))),
        }
    }

    pub(crate) fn transduce(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        reducer: &Reducer,
        cur_inst_span: &Span,
    ) -> Result<SteelVal> {
        let vm = Rc::new(RefCell::new(self));

        let mut nursery = None;

        let mut iter = Self::res_iterator(&root, &mut nursery)?;

        // TODO: Almost assuredly, this will not work with continuations, since we won't be
        // able to capture the rust stack. The best thing to do is probably to "reconstruct"
        // or pause the closures along the way, so that we could resume this step inside the
        // continuation at a later time if necessary.
        for t in ops {
            iter = match t {
                Transducers::Map(stack_func) => {
                    let vm_copy = Rc::clone(&vm);

                    // Inlining the switch on the function here saves a decent chunk
                    // of time since we don't have to dispatch on the function each time.
                    // Note: This optimization should also be copied to each of the below
                    // statements, but before that happens I want to figure out
                    // how to make this all work with continuations.
                    match stack_func {
                        SteelVal::FuncV(func) => Box::new(iter.map(move |arg| {
                            let arg_vec = [arg?];
                            func(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
                        })),
                        SteelVal::BoxedFunction(func) => Box::new(iter.map(move |arg| {
                            let arg_vec = [arg?];
                            func.func()(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
                        })),
                        SteelVal::MutFunc(func) => Box::new(iter.map(move |arg| {
                            let mut arg_vec = [arg?];
                            func(&mut arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
                        })),
                        // A closure that doesn't take one argument raises
                        // when it's first called, like any other call
                        SteelVal::Closure(closure) => {
                            if closure.is_multi_arity() {
                                Box::new(iter.map(move |arg| {
                                    vm_copy
                                        .borrow_mut()
                                        .call_with_one_arg_test::<true>(closure, arg?)
                                }))
                            } else {
                                Box::new(iter.map(move |arg| {
                                    vm_copy.borrow_mut().call_with_one_arg(closure, arg?)
                                }))
                            }
                        }
                        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
                    }
                }
                Transducers::Filter(stack_func) => {
                    let vm_copy = Rc::clone(&vm);

                    let switch_statement = move |arg: Result<SteelVal>| match arg {
                        Ok(arg) => {
                            let res = vm_copy.borrow_mut().call_func_or_else(
                                stack_func,
                                arg.clone(),
                                cur_inst_span,
                                throw!(TypeMismatch => "filter expected a function"; *cur_inst_span)
                            );

                            match res {
                                Ok(k) => match k {
                                    SteelVal::BoolV(false) => None,
                                    _ => Some(Ok(arg)),
                                },
                                Err(e) => Some(Err(e)),
                            }
                        }

                        _ => Some(arg),
                    };

                    Box::new(iter.filter_map(switch_statement))
                }
                Transducers::FlatMap(stack_func) => {
                    let vm_copy = Rc::clone(&vm);

                    let switch_statement =
                        move |arg: Result<SteelVal>| -> Box<dyn Iterator<Item = Result<SteelVal>>> {
                            match arg {
                                Ok(arg) => {
                                    let res = vm_copy.borrow_mut().call_func_or_else(
                                    stack_func,
                                    arg,
                                    cur_inst_span,
                                    throw!(TypeMismatch => "map expected a function"; *cur_inst_span),
                                );

                                    match res {
                                        Ok(x) => {
                                            Self::collection_iterator(x, cur_inst_span, "flatten")
                                        }
                                        err => Box::new(core::iter::once(err)),
                                    }
                                }

                                err => Box::new(core::iter::once(err)),
                            }
                        };

                    Box::new(iter.flat_map(switch_statement))
                }
                Transducers::Flatten => {
                    let switch_statement =
                        move |arg: Result<SteelVal>| -> Box<dyn Iterator<Item = Result<SteelVal>>> {
                            match arg {
                                Ok(x) => Self::collection_iterator(x, cur_inst_span, "flatten"),
                                err => Box::new(core::iter::once(err)),
                            }
                        };

                    Box::new(iter.flat_map(switch_statement))
                }
                // No function makes these
                Transducers::Window(_)
                | Transducers::TakeWhile(_)
                | Transducers::DropWhile(_)
                | Transducers::Cycle
                | Transducers::MapPair(_) => {
                    stop!(Generic => "transduce: this transducer is not implemented"; *cur_inst_span)
                }
                Transducers::Extend(collection) => {
                    let extender =
                        Self::collection_iterator(collection.clone(), cur_inst_span, "extending");

                    Box::new(iter.chain(extender))
                }
                Transducers::Take(num) => generate_take!(iter, num, cur_inst_span),
                Transducers::Drop(num) => generate_drop!(iter, num, cur_inst_span),
                Transducers::Enumerating => Box::new(iter.enumerate().map(|x| {
                    Ok(SteelVal::ListV(
                        vec![SteelVal::IntV(x.0 as isize), x.1?].into(),
                    ))
                })),
                Transducers::Zipping(collection) => {
                    let zipped =
                        Self::collection_iterator(collection.clone(), cur_inst_span, "zipping");
                    Box::new(
                        iter.zip(zipped)
                            .map(|x| Ok(SteelVal::ListV(vec![x.0?, x.1?].into()))),
                    )
                }
                Transducers::Interleaving(collection) => {
                    let other = Self::collection_iterator(
                        collection.clone(),
                        cur_inst_span,
                        "interleaving",
                    );
                    Box::new(interleave(iter, other))
                }
            }
        }

        Self::into_value(vm, reducer, iter, cur_inst_span)
    }

    fn into_value(
        vm_ctx: Rc<RefCell<&'global mut Self>>,
        reducer: &Reducer,
        mut iter: impl Iterator<Item = Result<SteelVal>>,
        cur_inst_span: &Span,
    ) -> Result<SteelVal> {
        match reducer {
            // The same arithmetic and comparisons as `+`, `*`, `max` and `min`
            Reducer::Sum => iter.try_fold(SteelVal::IntV(0), |acc, x| add_primitive(&[acc, x?])),
            Reducer::Multiply => {
                iter.try_fold(SteelVal::IntV(1), |acc, x| multiply_primitive(&[acc, x?]))
            }
            Reducer::Max | Reducer::Min => {
                let (name, better): (&str, fn(&[SteelVal]) -> Result<SteelVal>) =
                    if matches!(reducer, Reducer::Max) {
                        ("max", greater_than)
                    } else {
                        ("min", less_than)
                    };
                let mut best: Option<SteelVal> = None;
                for x in iter {
                    let x = x?;
                    best = Some(match best {
                        Some(b) if better(&[x.clone(), b.clone()])? != SteelVal::BoolV(true) => b,
                        _ => x,
                    });
                }
                best.ok_or_else(throw!(Generic => format!("`{name}` found empty list - `{name}` requires at least one element in the sequence")))
            }
            // Each of these stops at the first error, without running anything after it
            Reducer::Count => iter
                .try_fold(0isize, |count, x| x.map(|_| count + 1))
                .map(SteelVal::IntV),
            Reducer::Nth(n) => {
                for (i, x) in iter.enumerate() {
                    let x = x?;
                    if i == *n {
                        return Ok(x);
                    }
                }
                stop!(Generic => "`nth` - index given is greater than the length of the iterator")
            }
            Reducer::List => iter.collect::<Result<List<_>>>().map(SteelVal::ListV),
            Reducer::Vector => vec_construct_iter(iter),
            Reducer::HashMap => {
                iter.map(|x| {
                    match x? {
                        SteelVal::ListV(l) => {
                            if l.len() != 2 {
                                stop!(Generic => format!("Hashmap iterator expects an iterable with two elements, found: {}", SteelVal::ListV(l)));
                            } else {
                                let mut iter = l.into_iter();
                                Ok((iter.next().unwrap(), iter.next().unwrap()))
                            }
                        }
                        SteelVal::VectorV(l) => {
                            if l.len() != 2 {
                                stop!(Generic => format!("Hashmap iterator expects an iterable with two elements, found: {}", SteelVal::VectorV(l)));
                            } else {
                                let mut iter = l.iter();
                                Ok((iter.next().cloned().unwrap(), iter.next().cloned().unwrap()))
                            }
                        }
                        // TODO: Attempt to reuse the storage here
                        SteelVal::Pair(p) => {
                            Ok((p.car.clone(), p.cdr.clone()))
                        }
                        other => {
                            stop!(TypeMismatch => format!("Unable to convert: {other} to pair that can be used to construct a hashmap"));
                        }
                    }
                }).collect::<Result<HashMap<_, _>>>().map(|x| SteelVal::HashMapV(Gc::new(x).into()))
            },
            Reducer::HashSet => iter.collect::<Result<HashSet<_>>>().map(|x| SteelVal::HashSetV(Gc::new(x).into())),
            Reducer::String => {
                iter.map(|x| x.map(|x| {
                    // strings and chars need special treatment
                    // since their Display implementations emit quotes and hash-backslash respectively
                    match x {
                        SteelVal::StringV(s) => s.to_string(),
                        SteelVal::CharV(c) => c.to_string(),
                        x => x.to_string(),
                    }
                })).collect::<Result<String>>().map(|x| SteelVal::StringV(x.into()))
            },
            Reducer::Last => iter
                .try_fold(None, |_, x| x.map(Some))?
                .ok_or_else(throw!(Generic => "`last` found empty list - `last` requires at least one element in the sequence")),
            Reducer::ForEach(f) => {
                for value in iter {
                    vm_ctx.borrow_mut().call_func_or_else(
                        f,
                        value?,
                        cur_inst_span,
                        throw!(TypeMismatch => format!("for-each expected a function, found: {}", &f))
                    )?;
                }

                Ok(SteelVal::Void)
            },
            Reducer::Generic(reducer) => {
                iter.try_fold(reducer.initial_value.clone(), |acc, x| {
                    vm_ctx.borrow_mut().call_func_or_else_two_args(
                        &reducer.function,
                        acc,
                        x?,
                        cur_inst_span,
                        throw!(TypeMismatch => "reduce expected a function"; *cur_inst_span),
                    )
                })
            }
        }
    }
}
