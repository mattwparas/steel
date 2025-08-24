// use im_lists::list::List;
use crate::values::{lists::List, HashSet};
// use itertools::Itertools;

// use super::{evaluation_progress::EvaluationProgress, stack::StackFrame, vm::VmCore};
use super::{lazy_stream::LazyStreamIter, vm::VmCore};
use crate::{
    gc::Gc,
    parser::span::Span,
    primitives::vectors::vec_construct_iter,
    rerrs::{ErrorKind, SteelErr},
    rvals::{Result, SteelVal},
    stop,
    values::transducers::{Reducer, Transducers},
};

use crate::values::HashMap;
use std::{cell::RefCell, convert::TryInto};
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

impl<I, J> std::iter::FusedIterator for Interleave<I, J>
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
                stop!(ContractViolation => "take transducer must have a position number"; *$cur_inst_span)
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
                stop!(ContractViolation => "drop transducer must have a position number"; *$cur_inst_span)
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
        vm_ctx: Rc<RefCell<&'global mut Self>>,
        cur_inst_span: &'global Span,
        // The nursery here is for iterating over a vec since its wrapped inside the refcell
        nursery: &'global mut Option<Vec<SteelVal>>,
    ) -> Result<Box<dyn Iterator<Item = Result<SteelVal>> + 'global>> {
        match value {
            SteelVal::VectorV(v) => Ok(Box::new(v.iter().cloned().map(Ok))),
            SteelVal::StreamV(lazy_stream) => Ok(Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                vm_ctx,
                cur_inst_span,
            ))),
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

    pub(crate) fn transduce(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        reducer: &Reducer,
        cur_inst_span: &Span,
    ) -> Result<SteelVal> {
        let vm = Rc::new(RefCell::new(self));

        let mut nursery = None;

        let mut iter = Self::res_iterator(&root, Rc::clone(&vm), cur_inst_span, &mut nursery)?;

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
                        SteelVal::Closure(closure) => {
                            let multi_arity = if !closure.is_multi_arity() && closure.arity != 1 {
                                stop!(ArityMismatch => "map expects a function with one arg");
                            } else {
                                closure.is_multi_arity()
                            };

                            if multi_arity {
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
                                            match x {
                                                SteelVal::VectorV(v) => {
                                                    Box::new(v.0.unwrap().into_iter().map(Ok))
                                                }
                                                // TODO this needs to be fixed
                                                SteelVal::StringV(s) => Box::new(
                                                    s.chars()
                                                        .map(|x| Ok(SteelVal::CharV(x)))
                                                        .collect::<Vec<_>>()
                                                        .into_iter(),
                                                ),
                                                SteelVal::ListV(l) => {
                                                    Box::new(l.into_iter().map(Ok))
                                                }
                                                // SteelVal::StructV(s) => {
                                                //     Box::new(s.unwrap().fields.into_iter().map(Ok))
                                                // }
                                                els => {
                                                    let err = SteelErr::new(ErrorKind::TypeMismatch, format!("flatten expected a traversable value, found: {els}")).with_span(*cur_inst_span);

                                                    Box::new(std::iter::once(Err(err)))
                                                }
                                            }
                                        }
                                        err => Box::new(std::iter::once(err)),
                                    }
                                }

                                err => Box::new(std::iter::once(err)),
                            }
                        };

                    Box::new(iter.flat_map(switch_statement))
                }
                Transducers::Flatten => {
                    // TODO figure out how to use strings here
                    let switch_statement =
                        move |arg: Result<SteelVal>| -> Box<dyn Iterator<Item = Result<SteelVal>>> {
                            match arg {
                                Ok(x) => {
                                    match x {
                                        SteelVal::VectorV(v) => {
                                            Box::new(v.0.unwrap().into_iter().map(Ok))
                                        }
                                        // TODO this needs to be fixed
                                        SteelVal::StringV(s) => Box::new(
                                            s.chars()
                                                .map(|x| Ok(SteelVal::CharV(x)))
                                                .collect::<Vec<_>>()
                                                .into_iter(),
                                        ),
                                        SteelVal::ListV(l) => Box::new(l.into_iter().map(Ok)),
                                        // SteelVal::StructV(s) => {
                                        //     Box::new(s.unwrap().fields.into_iter().map(Ok))
                                        // }
                                        els => {
                                            let err = SteelErr::new(ErrorKind::TypeMismatch, format!("flatten expected a traversable value, found: {els}")).with_span(*cur_inst_span);

                                            Box::new(std::iter::once(Err(err)))
                                        }
                                    }
                                }
                                err => Box::new(std::iter::once(err)),
                            }
                        };

                    Box::new(iter.flat_map(switch_statement))

                    // todo!()
                }
                Transducers::Window(_num) => {
                    todo!()
                }
                Transducers::TakeWhile(_func) => {
                    todo!()
                }
                Transducers::DropWhile(_func) => {
                    todo!()
                }
                Transducers::Extend(collection) => {
                    let extender: Box<dyn Iterator<Item = Result<SteelVal>>> =
                        match collection.clone() {
                            SteelVal::VectorV(v) => Box::new(v.0.unwrap().into_iter().map(Ok)),
                            // TODO this needs to be fixed
                            SteelVal::StringV(s) => Box::new(
                                s.chars()
                                    .map(|x| Ok(SteelVal::CharV(x)))
                                    .collect::<Vec<_>>()
                                    .into_iter(),
                            ),
                            SteelVal::ListV(l) => Box::new(l.into_iter().map(Ok)),
                            // SteelVal::StructV(s) => Box::new(s.unwrap().fields.into_iter().map(Ok)),
                            els => {
                                let err = SteelErr::new(
                                    ErrorKind::TypeMismatch,
                                    format!("extending expected a traversable value, found: {els}"),
                                )
                                .with_span(*cur_inst_span);

                                Box::new(std::iter::once(Err(err)))
                            }
                        };

                    Box::new(iter.chain(extender))
                }
                Transducers::Cycle => {
                    todo!()
                }
                Transducers::Take(num) => generate_take!(iter, num, cur_inst_span),
                Transducers::Drop(num) => generate_drop!(iter, num, cur_inst_span),
                Transducers::Enumerating => Box::new(iter.enumerate().map(|x| {
                    Ok(SteelVal::ListV(
                        vec![SteelVal::IntV(x.0 as isize), x.1?].into(),
                    ))
                })),
                Transducers::Zipping(collection) => {
                    let zipped: Box<dyn Iterator<Item = Result<SteelVal>>> =
                        match collection.clone() {
                            SteelVal::VectorV(v) => Box::new(v.0.unwrap().into_iter().map(Ok)),
                            // TODO this needs to be fixed
                            SteelVal::StringV(s) => Box::new(
                                s.chars()
                                    .map(|x| Ok(SteelVal::CharV(x)))
                                    .collect::<Vec<_>>()
                                    .into_iter(),
                            ),
                            SteelVal::ListV(l) => Box::new(l.into_iter().map(Ok)),
                            // SteelVal::StructV(s) => Box::new(s.unwrap().fields.into_iter().map(Ok)),
                            els => {
                                let err = SteelErr::new(
                                    ErrorKind::TypeMismatch,
                                    format!("extending expected a traversable value, found: {els}"),
                                )
                                .with_span(*cur_inst_span);

                                Box::new(std::iter::once(Err(err)))
                            }
                        };
                    Box::new(
                        iter.zip(zipped)
                            .map(|x| Ok(SteelVal::ListV(vec![x.0?, x.1?].into()))),
                    )
                }
                Transducers::Interleaving(collection) => {
                    let other: Box<dyn Iterator<Item = Result<SteelVal>>> = match collection.clone()
                    {
                        SteelVal::VectorV(v) => Box::new(v.0.unwrap().into_iter().map(Ok)),
                        // TODO this needs to be fixed
                        SteelVal::StringV(s) => Box::new(
                            s.chars()
                                .map(|x| Ok(SteelVal::CharV(x)))
                                .collect::<Vec<_>>()
                                .into_iter(),
                        ),
                        SteelVal::ListV(l) => Box::new(l.into_iter().map(Ok)),
                        // SteelVal::StructV(s) => Box::new(s.unwrap().fields.into_iter().map(Ok)),
                        els => {
                            let err = SteelErr::new(
                                ErrorKind::TypeMismatch,
                                format!("extending expected a traversable value, found: {els}"),
                            )
                            .with_span(*cur_inst_span);

                            Box::new(std::iter::once(Err(err)))
                        }
                    };
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
            // TODO this only works with integer values right now
            Reducer::Sum => {
                iter.map(|x| match x? {
                    SteelVal::IntV(v) => {
                        Ok(v)
                    }
                    other => {
                        stop!(TypeMismatch => "sum expects an integer value, found: {:?}", other)
                    }
                })
                .sum::<Result<isize>>()
                .map(SteelVal::IntV)
            },
            Reducer::Multiply => {
                iter.map(|x| match x? {
                    SteelVal::IntV(v) => {
                        Ok(v)
                    }
                    other => {
                        stop!(TypeMismatch => "sum expects an integer value, found: {:?}", other)
                    }
                })
                .product::<Result<isize>>()
                .map(SteelVal::IntV)
            }
            Reducer::Max => todo!(),
            Reducer::Min => todo!(),
            Reducer::Count => {
                Ok(SteelVal::IntV(iter.count().try_into().unwrap())) // TODO have proper big int
            },
            Reducer::Nth(usize) => {
                iter.nth(*usize).unwrap_or_else(|| stop!(Generic => "`nth` - index given is greater than the length of the iterator"))
            },
            Reducer::List => iter.collect::<Result<List<_>>>().map(SteelVal::ListV),
            Reducer::Vector => vec_construct_iter(iter),
            Reducer::HashMap => {
                iter.map(|x| {
                    match x? {
                        SteelVal::ListV(l) => {
                            if l.len() != 2 {
                                stop!(Generic => format!("Hashmap iterator expects an iterable with two elements, found: {l:?}"));
                            } else {
                                let mut iter = l.into_iter();
                                Ok((iter.next().unwrap(), iter.next().unwrap()))
                            }
                        }
                        SteelVal::VectorV(l) => {
                            if l.len() != 2 {
                                stop!(Generic => format!("Hashmap iterator expects an iterable with two elements, found: {:?}", &l.0));
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
            Reducer::Last => iter.last().unwrap_or_else(|| stop!(Generic => "`last` found empty list - `last` requires at least one element in the sequence")),
            Reducer::ForEach(f) => {
                for value in iter {
                    vm_ctx.borrow_mut().call_func_or_else(
                        &f,
                        value?,
                        cur_inst_span,
                        throw!(TypeMismatch => format!("for-each expected a function, found: {}", &f))
                    )?;
                }

                Ok(SteelVal::Void)
            },
            Reducer::Generic(reducer) => {

                let initial_value = Ok(reducer.initial_value.clone());

                let switch_statement = move |acc, x| {
                    vm_ctx.borrow_mut().call_func_or_else_two_args(
                        &reducer.function,
                        acc?,
                        x?,
                        cur_inst_span,
                        throw!(TypeMismatch => "reduce expected a function"; *cur_inst_span),
                    )
                };

                iter.fold(initial_value, switch_statement)
            }
        }
    }
}
