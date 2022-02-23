use im_lists::list::List;

// use super::{evaluation_progress::EvaluationProgress, stack::StackFrame, vm::VmCore};
use super::{
    lazy_stream::LazyStreamIter,
    options::{ApplyContracts, UseCallbacks},
    vm::{VmContext, VmCore},
};
use crate::{
    compiler::constants::ConstantTable,
    gc::Gc,
    parser::span::Span,
    primitives::VectorOperations,
    rerrs::{ErrorKind, SteelErr},
    rvals::{Result, SteelVal},
    stop,
    values::transducers::{Reducer, Transducers},
};

use std::{cell::Ref, rc::Rc};
use std::{cell::RefCell, convert::TryInto};

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

pub(crate) const TRANSDUCE: SteelVal = SteelVal::BuiltIn(transduce);

// figure out if nested transducers works
fn transduce(mut args: Vec<SteelVal>, ctx: &mut dyn VmContext) -> Result<SteelVal> {
    let reducer = args
        .pop()
        .ok_or_else(throw!(ArityMismatch => "transduce expects 3 arguments, found none"))?;
    let mut arg_iter = args.into_iter();
    let collection = arg_iter.next().unwrap();

    // TODO make this way better
    let transducers: Vec<_> = arg_iter
        .map(|x| {
            if let SteelVal::IterV(i) = x {
                Ok(i)
            } else {
                stop!(TypeMismatch => format!("transduce expects a transducer, found: {}", x))
            }
        })
        .collect::<Result<Vec<_>>>()?;

    let transducers = transducers
        .into_iter()
        .flat_map(|x| x.ops.clone())
        .collect::<Vec<_>>();

    if let SteelVal::ReducerV(r) = &reducer {
        // TODO get rid of this unwrap
        // just pass a reference instead
        ctx.call_transduce(&transducers, collection, r.unwrap())
    } else {
        stop!(TypeMismatch => format!("transduce requires that the last argument be a reducer, found: {}", reducer))
    }
}

impl<'global, 'a, CT: ConstantTable, U: UseCallbacks, A: ApplyContracts> VmCore<'a, CT, U, A> {
    // With transducers, we also need reducers
    // reducers should define _how_ a value is going to be converted away
    // from the iterator stream
    // This could either be a function that returns a single value, or a generic collection type
    pub(crate) fn res_iterator(
        value: &'global SteelVal,
        vm_ctx: Rc<RefCell<&'global mut Self>>,
        cur_inst_span: &'global Span,
    ) -> Result<Box<dyn Iterator<Item = Result<SteelVal>> + 'global>> {
        match value {
            SteelVal::VectorV(v) => Ok(Box::new(v.iter().cloned().map(Ok))),
            SteelVal::StreamV(lazy_stream) => Ok(Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                vm_ctx,
                cur_inst_span,
            ))),
            SteelVal::StringV(s) => Ok(Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x))))),
            SteelVal::ListV(l) => Ok(Box::new(l.iter().cloned().map(Ok))),
            SteelVal::StructV(s) => Ok(Box::new(s.iter().cloned().map(Ok))),
            SteelVal::HashSetV(hs) => Ok(Box::new(hs.iter().cloned().map(Ok))),
            SteelVal::HashMapV(hm) => {
                Ok(Box::new(hm.iter().map(|x| {
                    Ok(SteelVal::ListV(im_lists::list![x.0.clone(), x.1.clone()]))
                })))
            }
            // SteelVal::MutableVector(v) => {
            //     Ok(Box::new(
            //         Ref::map(v.borrow(), |x| &x.iter()).cloned().map(Ok),
            //     ))
            //     // Ok(Box::new(ref_borrow))
            // }
            _ => {
                stop!(TypeMismatch => format!("value unable to be converted to an iterable: {}", value))
            }
        }
    }

    pub(crate) fn transduce(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        reducer: Reducer,
        cur_inst_span: &Span,
    ) -> Result<SteelVal> {
        let vm = Rc::new(RefCell::new(self));

        let mut iter = Self::res_iterator(&root, Rc::clone(&vm), cur_inst_span)?;

        for t in ops {
            iter = match t {
                Transducers::Map(stack_func) => {
                    let vm_copy = Rc::clone(&vm);

                    let switch_statement = move |arg| {
                        vm_copy.borrow_mut().call_func_or_else(
                            stack_func,
                            arg?,
                            cur_inst_span,
                            throw!(TypeMismatch => "map expected a function"; *cur_inst_span),
                        )
                    };

                    Box::new(iter.map(switch_statement))
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
                                    SteelVal::BoolV(true) => Some(Ok(arg)),
                                    SteelVal::BoolV(false) => None,
                                    _ => None,
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
                                                    Box::new(v.unwrap().into_iter().map(Ok))
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
                                                SteelVal::StructV(s) => {
                                                    Box::new(s.unwrap().fields.into_iter().map(Ok))
                                                }
                                                els => {
                                                    let err = SteelErr::new(ErrorKind::TypeMismatch, format!("flatten expected a traversable value, found: {}", els)).with_span(*cur_inst_span);

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
                                            Box::new(v.unwrap().into_iter().map(Ok))
                                        }
                                        // TODO this needs to be fixed
                                        SteelVal::StringV(s) => Box::new(
                                            s.chars()
                                                .map(|x| Ok(SteelVal::CharV(x)))
                                                .collect::<Vec<_>>()
                                                .into_iter(),
                                        ),
                                        SteelVal::ListV(l) => Box::new(l.into_iter().map(Ok)),
                                        SteelVal::StructV(s) => {
                                            Box::new(s.unwrap().fields.into_iter().map(Ok))
                                        }
                                        els => {
                                            let err = SteelErr::new(ErrorKind::TypeMismatch, format!("flatten expected a traversable value, found: {}", els)).with_span(*cur_inst_span);

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
                Transducers::Window(num) => {
                    todo!()
                }
                Transducers::TakeWhile(func) => {
                    todo!()
                }
                Transducers::DropWhile(func) => {
                    todo!()
                }
                Transducers::Extend(collection) => {
                    let extender: Box<dyn Iterator<Item = Result<SteelVal>>> = match collection
                        .clone()
                    {
                        SteelVal::VectorV(v) => Box::new(v.unwrap().into_iter().map(Ok)),
                        // TODO this needs to be fixed
                        SteelVal::StringV(s) => Box::new(
                            s.chars()
                                .map(|x| Ok(SteelVal::CharV(x)))
                                .collect::<Vec<_>>()
                                .into_iter(),
                        ),
                        SteelVal::ListV(l) => Box::new(l.into_iter().map(Ok)),
                        SteelVal::StructV(s) => Box::new(s.unwrap().fields.into_iter().map(Ok)),
                        els => {
                            let err = SteelErr::new(
                                ErrorKind::TypeMismatch,
                                format!("extending expected a traversable value, found: {}", els),
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
            }
        }

        Self::into_value(vm, reducer, iter, cur_inst_span)
    }

    fn into_value(
        vm_ctx: Rc<RefCell<&'global mut Self>>,
        reducer: Reducer,
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
                iter.nth(usize).unwrap_or_else(|| stop!(Generic => "`nth` - index given is greater than the length of the iterator"))
            },
            Reducer::List => iter.collect::<Result<List<_>>>().map(SteelVal::ListV),
            Reducer::Vector => VectorOperations::vec_construct_iter(iter),
            Reducer::HashMap => {
                iter.map(|x| {
                    match x? {
                        SteelVal::ListV(l) => {
                            if l.len() != 2 {
                                stop!(Generic => format!("Hashmap iterator expects an iterable with two elements, found: {:?}", l));
                            } else {
                                let mut iter = l.into_iter();
                                Ok((iter.next().unwrap(), iter.next().unwrap()))
                            }
                        }
                        SteelVal::VectorV(l) => {
                            if l.len() != 2 {
                                stop!(Generic => format!("Hashmap iterator expects an iterable with two elements, found: {:?}", l));
                            } else {
                                let mut iter = l.iter();
                                Ok((iter.next().cloned().unwrap(), iter.next().cloned().unwrap()))
                            }
                        }
                        other => {
                            stop!(TypeMismatch => format!("Unable to convert: {} to pair that can be used to construct a hashmap", other));
                        }
                    }
                }).collect::<Result<im_rc::HashMap<_, _>>>().map(|x| SteelVal::HashMapV(Gc::new(x)))
            },
            Reducer::HashSet => iter.collect::<Result<im_rc::HashSet<_>>>().map(|x| SteelVal::HashSetV(Gc::new(x))),
            Reducer::String => todo!(),
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
