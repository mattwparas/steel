use super::{evaluation_progress::EvaluationProgress, stack::StackFrame, vm::VmCore};
use crate::{
    compiler::constants::ConstantTable,
    parser::span::Span,
    primitives::{ListOperations, VectorOperations},
    rerrs::{ErrorKind, SteelErr},
    rvals::{CollectionType, Result, SteelVal, Transducer, Transducers},
    stop,
};

use std::cell::RefCell;
use std::rc::Rc;

use super::heap::Heap;
use super::heap2::UpValueHeap;
use crate::env::Env;

use super::contracts::ContractedFunctionExt;

use super::vm::vm;
use crate::gc::Gc;

use super::inline_iter::*;
use super::lazy_stream::LazyStreamIter;

impl<'a, CT: ConstantTable> VmCore<'a, CT> {
    pub(crate) fn run(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        collection_type: Option<SteelVal>,
        cur_inst_span: &Span,
    ) -> Result<SteelVal> {
        // By default, match the output type to the input type
        let output_type = match root {
            SteelVal::VectorV(_) => CollectionType::Vector,
            _ => CollectionType::List,
        };

        // Initialize the iterator to be the iterator over whatever is given, stop if its not iterable
        let mut iter: Box<dyn Iterator<Item = Result<SteelVal>>> = match &root {
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(|x| Ok(x))),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                self.constants,
                cur_inst_span,
                self.callback,
            )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        let constants = self.constants;
        let callback = self.callback;

        for t in ops {
            iter = match t {
                Transducers::Map(stack_func) => {
                    let switch_statement = move |arg| match &stack_func {
                        SteelVal::FuncV(func) => {
                            let arg_vec = [arg?];
                            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
                        }
                        SteelVal::BoxedFunction(func) => {
                            let arg_vec = [arg?];
                            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
                        }
                        // SteelVal::StructClosureV(sc) => {
                        //     let arg_vec = vec![arg?];
                        //     (sc.func)(&arg_vec, &sc.factory).map_err(|x| x.set_span(*cur_inst_span))
                        // }
                        SteelVal::ContractedFunction(cf) => {
                            let arg_vec = vec![arg?];
                            let mut local_heap = Heap::new();
                            let mut local_upvalue_heap = UpValueHeap::new();
                            cf.apply(
                                arg_vec,
                                &mut local_heap,
                                constants,
                                cur_inst_span,
                                callback,
                                &mut local_upvalue_heap,
                            )
                        }
                        SteelVal::Closure(closure) => {
                            // ignore the stack limit here
                            let args = vec![arg?];
                            // if let Some()

                            let parent_env = closure.sub_expression_env();

                            // TODO remove this unwrap
                            let offset = closure.offset()
                                + parent_env.upgrade().unwrap().borrow().local_offset();

                            let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                                parent_env.clone(),
                                offset,
                            )));

                            inner_env
                                .borrow_mut()
                                .reserve_defs(if closure.ndef_body() > 0 {
                                    closure.ndef_body() - 1
                                } else {
                                    0
                                });

                            let mut local_heap = Heap::new();
                            let mut local_upvalue_heap = UpValueHeap::new();

                            // TODO make recursive call here with a very small stack
                            // probably a bit overkill, but not much else I can do here I think
                            vm(
                                closure.body_exp(),
                                args.into(),
                                &mut local_heap,
                                inner_env,
                                constants,
                                callback,
                                &mut local_upvalue_heap,
                                vec![Gc::clone(closure)],
                            )
                        }
                        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
                    };

                    Box::new(iter.map(switch_statement))
                }
                // Box::new(inline_map_result_iter(
                //     iter,
                //     func.clone(),
                //     self.constants,
                //     &cur_inst_span,
                //     self.callback,
                //     &mut self.stack,
                // )),
                Transducers::Filter(stack_func) => {
                    let switch_statement = move |arg: Result<SteelVal>| match arg {
                        Ok(arg) => {
                            match stack_func {
                                SteelVal::FuncV(func) => {
                                    let arg_vec = [arg.clone()];
                                    let res =
                                        func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span));
                                    match res {
                                        Ok(k) => match k {
                                            SteelVal::BoolV(true) => Some(Ok(arg)),
                                            SteelVal::BoolV(false) => None,
                                            _ => None,
                                        },
                                        Err(e) => Some(Err(e)),
                                        // _ => None,
                                    }
                                }
                                SteelVal::BoxedFunction(func) => {
                                    let arg_vec = [arg.clone()];
                                    let res =
                                        func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span));
                                    match res {
                                        Ok(k) => match k {
                                            SteelVal::BoolV(true) => Some(Ok(arg)),
                                            SteelVal::BoolV(false) => None,
                                            _ => None,
                                        },
                                        Err(e) => Some(Err(e)),
                                        // _ => None,
                                    }
                                }
                                SteelVal::ContractedFunction(cf) => {
                                    let arg_vec = vec![arg.clone()];
                                    let mut local_heap = Heap::new();
                                    let mut local_upvalue_heap = UpValueHeap::new();
                                    let res = cf.apply(
                                        arg_vec,
                                        &mut local_heap,
                                        constants,
                                        cur_inst_span,
                                        callback,
                                        &mut local_upvalue_heap,
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
                                SteelVal::Closure(closure) => {
                                    // ignore the stack limit here
                                    let args = vec![arg.clone()];
                                    // if let Some()

                                    let parent_env = closure.sub_expression_env();

                                    let offset = closure.offset()
                                        + parent_env.upgrade().unwrap().borrow().local_offset();

                                    let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                                        parent_env.clone(),
                                        offset,
                                    )));

                                    inner_env.borrow_mut().reserve_defs(
                                        if closure.ndef_body() > 0 {
                                            closure.ndef_body() - 1
                                        } else {
                                            0
                                        },
                                    );

                                    let mut local_heap = Heap::new();
                                    let mut local_upvalue_heap = UpValueHeap::new();

                                    // TODO make recursive call here with a very small stack
                                    // probably a bit overkill, but not much else I can do here I think
                                    let res = vm(
                                        closure.body_exp(),
                                        args.into(),
                                        &mut local_heap,
                                        inner_env,
                                        constants,
                                        callback,
                                        &mut local_upvalue_heap,
                                        vec![Gc::clone(closure)],
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
                                _ => Some(Err(SteelErr::new(
                                    ErrorKind::TypeMismatch,
                                    "map expected a function".to_string(),
                                )
                                .with_span(*cur_inst_span))),
                            }
                        }

                        _ => Some(arg),
                    };

                    Box::new(iter.filter_map(switch_statement))
                }

                // Box::new(inline_filter_result_iter(
                //     iter,
                //     func.clone(),
                //     self.constants,
                //     &cur_inst_span,
                //     self.callback,
                //     &mut self.stack,
                // )),
                Transducers::Take(num) => {
                    if let SteelVal::IntV(num) = num {
                        if *num < 0 {
                            stop!(ContractViolation => "take transducer must have a position number"; *cur_inst_span)
                        }
                        Box::new(iter.take(*num as usize))
                    } else {
                        stop!(TypeMismatch => "take transducer takes an integer"; *cur_inst_span)
                    }
                }
                Transducers::Drop(num) => {
                    if let SteelVal::IntV(num) = num {
                        if *num < 0 {
                            stop!(ContractViolation => "drop transducer must have a position number"; *cur_inst_span)
                        }
                        Box::new(iter.skip(*num as usize))
                    } else {
                        stop!(TypeMismatch => "drop transducer takes an integer"; *cur_inst_span)
                    }
                }
            }
        }

        // If an output type is given, use that one
        if let Some(collection_type) = collection_type {
            if let SteelVal::SymbolV(n) = collection_type {
                match n.as_ref() {
                    "list" => ListOperations::built_in_list_normal_iter(iter),
                    "vector" => VectorOperations::vec_construct_iter(iter),
                    _ => stop!(Generic => "Cannot collect into an undefined type"),
                }
            } else {
                stop!(Generic => "execute takes a symbol")
            }
        } else {
            match output_type {
                CollectionType::List => ListOperations::built_in_list_normal_iter(iter),
                CollectionType::Vector => VectorOperations::vec_construct_iter(iter),
            }
        }
    }

    pub(crate) fn transduce(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        initial_value: SteelVal,
        reducer: SteelVal,
        cur_inst_span: &Span,
    ) -> Result<SteelVal> {
        let mut iter: Box<dyn Iterator<Item = Result<SteelVal>>> = match &root {
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(|x| Ok(x))),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                self.constants,
                cur_inst_span,
                self.callback,
            )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        for t in ops {
            // my_iter = t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;

            iter = match t {
                Transducers::Map(func) => Box::new(inline_map_result_iter(
                    iter,
                    func.clone(),
                    self.constants,
                    &cur_inst_span,
                    self.callback,
                    &mut self.stack,
                )),
                Transducers::Filter(func) => Box::new(inline_filter_result_iter(
                    iter,
                    func.clone(),
                    self.constants,
                    &cur_inst_span,
                    self.callback,
                    &mut self.stack,
                )),
                Transducers::Take(num) => {
                    if let SteelVal::IntV(num) = num {
                        if *num < 0 {
                            stop!(ContractViolation => "take transducer must have a position number"; *cur_inst_span)
                        }
                        Box::new(iter.take(*num as usize))
                    } else {
                        stop!(TypeMismatch => "take transducer takes an integer"; *cur_inst_span)
                    }
                }
                Transducers::Drop(num) => {
                    if let SteelVal::IntV(num) = num {
                        if *num < 0 {
                            stop!(ContractViolation => "drop transducer must have a position number"; *cur_inst_span)
                        }
                        Box::new(iter.skip(*num as usize))
                    } else {
                        stop!(TypeMismatch => "drop transducer takes an integer"; *cur_inst_span)
                    }
                }
            }
        }

        inline_reduce_iter(
            iter,
            initial_value,
            reducer,
            self.constants,
            cur_inst_span,
            self.callback,
            &mut self.stack,
        )
    }
}
