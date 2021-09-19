use im_lists::list::List;

// use super::{evaluation_progress::EvaluationProgress, stack::StackFrame, vm::VmCore};
use super::{
    options::{ApplyContracts, UseCallbacks},
    vm::VmCore,
};
use crate::{
    compiler::constants::ConstantTable,
    parser::span::Span,
    primitives::{ListOperations, VectorOperations},
    rerrs::{ErrorKind, SteelErr},
    rvals::{CollectionType, Result, SteelVal, Transducers},
    stop,
};

use std::cell::RefCell;
use std::rc::Rc;

// use super::contracts::ContractedFunctionExt;

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

// trait Output

impl<'a, CT: ConstantTable, U: UseCallbacks, A: ApplyContracts> VmCore<'a, CT, U, A> {
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

        // let global_env: Rc<RefCell<&mut &'a mut Env>> = Rc::new(RefCell::new(&mut self.global_env));

        // Initialize the iterator to be the iterator over whatever is given, stop if its not iterable
        let mut iter: Box<dyn Iterator<Item = Result<SteelVal>>> = match &root {
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(Ok)),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(Ok)),
            // SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
            //     lazy_stream.unwrap(),
            //     self.constants,
            //     cur_inst_span,
            //     self.callback,
            //     Rc::clone(&global_env),
            //     self.use_callbacks,
            //     self.apply_contracts,
            // )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            SteelVal::ListV(l) => Box::new(l.iter().cloned().map(Ok)),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        let vm = Rc::new(RefCell::new(self));

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
                    todo!()
                }
                Transducers::Flatten => {
                    todo!()
                }
                Transducers::Window(num) => {
                    todo!()
                }
                Transducers::Take(num) => generate_take!(iter, num, cur_inst_span),
                Transducers::Drop(num) => generate_drop!(iter, num, cur_inst_span),
            }
        }

        // If an output type is given, use that one
        if let Some(collection_type) = collection_type {
            if let SteelVal::SymbolV(n) = collection_type {
                match n.as_ref() {
                    "list" => ListOperations::built_in_list_normal_iter(iter),
                    "vector" => VectorOperations::vec_construct_iter(iter),
                    "test-list" => iter.collect::<Result<List<_>>>().map(SteelVal::ListV),
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
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(Ok)),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(Ok)),
            // SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
            //     lazy_stream.unwrap(),
            //     self.constants,
            //     cur_inst_span,
            //     self.callback,
            //     Rc::clone(&global_env),
            //     self.use_callbacks,
            //     self.apply_contracts,
            // )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            SteelVal::ListV(l) => Box::new(l.iter().cloned().map(Ok)),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        let vm = Rc::new(RefCell::new(self));

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
                    todo!()
                }
                Transducers::Flatten => {
                    todo!()
                }
                Transducers::Window(num) => {
                    todo!()
                }
                Transducers::Take(num) => generate_take!(iter, num, cur_inst_span),
                Transducers::Drop(num) => generate_drop!(iter, num, cur_inst_span),
            }
        }

        let vm_copy = Rc::clone(&vm);

        let switch_statement = move |acc, x| {
            vm_copy.borrow_mut().call_func_or_else_two_args(
                &reducer,
                acc?,
                x?,
                cur_inst_span,
                throw!(TypeMismatch => "reduce expected a function"; *cur_inst_span),
            )
        };

        iter.fold(Ok(initial_value), switch_statement)
    }
}
