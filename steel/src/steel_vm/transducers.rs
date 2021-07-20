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

use super::heap::UpValueHeap;
use crate::env::Env;

use super::contracts::ContractedFunctionExt;

use super::vm::vm;
use crate::gc::Gc;

// use super::inline_iter::*;
use super::lazy_stream::LazyStreamIter;

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

        let global_env: Rc<RefCell<&mut &'a mut Env>> = Rc::new(RefCell::new(&mut self.global_env));

        // Initialize the iterator to be the iterator over whatever is given, stop if its not iterable
        let mut iter: Box<dyn Iterator<Item = Result<SteelVal>>> = match &root {
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(Ok)),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(Ok)),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                self.constants,
                cur_inst_span,
                self.callback,
                Rc::clone(&global_env),
                self.use_callbacks,
                self.apply_contracts,
            )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        let constants = self.constants;
        let callback = self.callback;
        let vm_stack = Rc::new(RefCell::new(&mut self.stack));
        let vm_stack_index = Rc::new(RefCell::new(&mut self.stack_index));
        let function_stack = Rc::new(RefCell::new(&mut self.function_stack));

        let use_callbacks = self.use_callbacks;
        let apply_contracts = self.apply_contracts;

        for t in ops {
            iter = match t {
                Transducers::Map(stack_func) => {
                    let vm_stack_copy = Rc::clone(&vm_stack);
                    let vm_stack_index_copy = Rc::clone(&vm_stack_index);
                    let function_stack_copy = Rc::clone(&function_stack);
                    let global_env_copy = Rc::clone(&global_env);

                    let switch_statement = move |arg| match &stack_func {
                        SteelVal::FuncV(func) => {
                            let arg_vec = [arg?];
                            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
                        }
                        SteelVal::BoxedFunction(func) => {
                            let arg_vec = [arg?];
                            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
                        }
                        SteelVal::ContractedFunction(cf) => {
                            let arg_vec = vec![arg?];
                            let mut local_upvalue_heap = UpValueHeap::new();
                            cf.apply(
                                arg_vec,
                                constants,
                                cur_inst_span,
                                callback,
                                &mut local_upvalue_heap,
                                &mut global_env_copy.borrow_mut(),
                                &mut vm_stack_copy.borrow_mut(),
                                &mut function_stack_copy.borrow_mut(),
                                &mut vm_stack_index_copy.borrow_mut(),
                                use_callbacks,
                                apply_contracts,
                            )
                        }
                        SteelVal::Closure(closure) => {
                            let mut local_upvalue_heap = UpValueHeap::new();

                            // Set the state prior to the recursive call
                            vm_stack_index_copy
                                .borrow_mut()
                                .push(vm_stack_copy.borrow().len());

                            vm_stack_copy.borrow_mut().push(arg?);

                            function_stack_copy.borrow_mut().push(Gc::clone(closure));

                            // println!("Calling vm inside map");

                            // TODO make recursive call here with a very small stack
                            // probably a bit overkill, but not much else I can do here I think
                            let output = vm(
                                closure.body_exp(),
                                &mut vm_stack_copy.borrow_mut(),
                                &mut global_env_copy.borrow_mut(),
                                constants,
                                callback,
                                &mut local_upvalue_heap,
                                &mut function_stack_copy.borrow_mut(),
                                &mut vm_stack_index_copy.borrow_mut(),
                                use_callbacks,
                                apply_contracts,
                                None,
                            );

                            output
                        }
                        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
                    };

                    Box::new(iter.map(switch_statement))
                }
                Transducers::Filter(stack_func) => {
                    let vm_stack_copy = Rc::clone(&vm_stack);
                    let vm_stack_index_copy = Rc::clone(&vm_stack_index);
                    let function_stack_copy = Rc::clone(&function_stack);
                    let global_env_copy = Rc::clone(&global_env);

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
                                    let mut local_upvalue_heap = UpValueHeap::new();
                                    let res = cf.apply(
                                        arg_vec,
                                        constants,
                                        cur_inst_span,
                                        callback,
                                        &mut local_upvalue_heap,
                                        &mut global_env_copy.borrow_mut(),
                                        &mut vm_stack_copy.borrow_mut(),
                                        &mut function_stack_copy.borrow_mut(),
                                        &mut vm_stack_index_copy.borrow_mut(),
                                        use_callbacks,
                                        apply_contracts,
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
                                    let mut local_upvalue_heap = UpValueHeap::new();

                                    // Set the state prior to the recursive call
                                    vm_stack_index_copy
                                        .borrow_mut()
                                        .push(vm_stack_copy.borrow().len());

                                    vm_stack_copy.borrow_mut().push(arg.clone());

                                    function_stack_copy.borrow_mut().push(Gc::clone(closure));

                                    // TODO make recursive call here with a very small stack
                                    // probably a bit overkill, but not much else I can do here I think
                                    let res = vm(
                                        closure.body_exp(),
                                        &mut vm_stack_copy.borrow_mut(),
                                        &mut global_env_copy.borrow_mut(),
                                        constants,
                                        callback,
                                        &mut local_upvalue_heap,
                                        &mut function_stack_copy.borrow_mut(),
                                        &mut vm_stack_index_copy.borrow_mut(),
                                        use_callbacks,
                                        apply_contracts,
                                        None,
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
                                    "filter expected a function".to_string(),
                                )
                                .with_span(*cur_inst_span))),
                            }
                        }

                        _ => Some(arg),
                    };

                    Box::new(iter.filter_map(switch_statement))
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
        let constants = self.constants;
        let callback = self.callback;
        let vm_stack = Rc::new(RefCell::new(&mut self.stack));
        let vm_stack_index = Rc::new(RefCell::new(&mut self.stack_index));
        let function_stack = Rc::new(RefCell::new(&mut self.function_stack));
        let heap = Rc::new(RefCell::new(&mut self.upvalue_heap));

        let global_env = Rc::new(RefCell::new(&mut self.global_env));

        let mut iter: Box<dyn Iterator<Item = Result<SteelVal>>> = match &root {
            SteelVal::VectorV(v) => Box::new(v.iter().cloned().map(Ok)),
            SteelVal::Pair(_) => Box::new(SteelVal::iter(root).into_iter().map(Ok)),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.unwrap(),
                self.constants,
                cur_inst_span,
                self.callback,
                Rc::clone(&global_env),
                self.use_callbacks,
                self.apply_contracts,
            )),
            SteelVal::StringV(s) => Box::new(s.chars().map(|x| Ok(SteelVal::CharV(x)))),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        let use_callbacks = self.use_callbacks;
        let apply_contracts = self.apply_contracts;

        for t in ops {
            iter = match t {
                Transducers::Map(stack_func) => {
                    let vm_stack_copy = Rc::clone(&vm_stack);
                    let vm_stack_index_copy = Rc::clone(&vm_stack_index);
                    let function_stack_copy = Rc::clone(&function_stack);
                    let global_env_copy = Rc::clone(&global_env);
                    let heap_copy = Rc::clone(&heap);

                    let switch_statement = move |arg| match &stack_func {
                        SteelVal::FuncV(func) => {
                            let arg_vec = [arg?];
                            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
                        }
                        SteelVal::BoxedFunction(func) => {
                            let arg_vec = [arg?];
                            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
                        }
                        SteelVal::ContractedFunction(cf) => {
                            let arg_vec = vec![arg?];
                            cf.apply(
                                arg_vec,
                                constants,
                                cur_inst_span,
                                callback,
                                &mut heap_copy.borrow_mut(),
                                &mut global_env_copy.borrow_mut(),
                                &mut vm_stack_copy.borrow_mut(),
                                &mut function_stack_copy.borrow_mut(),
                                &mut vm_stack_index_copy.borrow_mut(),
                                use_callbacks,
                                apply_contracts,
                            )
                        }
                        SteelVal::Closure(closure) => {
                            // Set the state prior to the recursive call
                            vm_stack_index_copy
                                .borrow_mut()
                                .push(vm_stack_copy.borrow().len());

                            vm_stack_copy.borrow_mut().push(arg?);

                            function_stack_copy.borrow_mut().push(Gc::clone(closure));

                            // TODO make recursive call here with a very small stack
                            // probably a bit overkill, but not much else I can do here I think
                            let output = vm(
                                closure.body_exp(),
                                &mut vm_stack_copy.borrow_mut(),
                                &mut global_env_copy.borrow_mut(),
                                constants,
                                callback,
                                &mut heap_copy.borrow_mut(),
                                &mut function_stack_copy.borrow_mut(),
                                &mut vm_stack_index_copy.borrow_mut(),
                                use_callbacks,
                                apply_contracts,
                                None,
                            );

                            output
                        }
                        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
                    };

                    Box::new(iter.map(switch_statement))
                }
                Transducers::Filter(stack_func) => {
                    let vm_stack_copy = Rc::clone(&vm_stack);
                    let vm_stack_index_copy = Rc::clone(&vm_stack_index);
                    let function_stack_copy = Rc::clone(&function_stack);
                    let global_env_copy = Rc::clone(&global_env);
                    let heap_copy = Rc::clone(&heap);

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
                                    let res = cf.apply(
                                        arg_vec,
                                        constants,
                                        cur_inst_span,
                                        callback,
                                        &mut heap_copy.borrow_mut(),
                                        &mut global_env_copy.borrow_mut(),
                                        &mut vm_stack_copy.borrow_mut(),
                                        &mut function_stack_copy.borrow_mut(),
                                        &mut vm_stack_index_copy.borrow_mut(),
                                        use_callbacks,
                                        apply_contracts,
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
                                    // Set the state prior to the recursive call
                                    vm_stack_index_copy
                                        .borrow_mut()
                                        .push(vm_stack_copy.borrow().len());

                                    vm_stack_copy.borrow_mut().push(arg.clone());

                                    function_stack_copy.borrow_mut().push(Gc::clone(closure));

                                    // TODO make recursive call here with a very small stack
                                    // probably a bit overkill, but not much else I can do here I think
                                    let res = vm(
                                        closure.body_exp(),
                                        &mut vm_stack_copy.borrow_mut(),
                                        &mut global_env_copy.borrow_mut(),
                                        constants,
                                        callback,
                                        &mut heap_copy.borrow_mut(),
                                        &mut function_stack_copy.borrow_mut(),
                                        &mut vm_stack_index_copy.borrow_mut(),
                                        use_callbacks,
                                        apply_contracts,
                                        None,
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
                                    "filter expected a function".to_string(),
                                )
                                .with_span(*cur_inst_span))),
                            }
                        }

                        _ => Some(arg),
                    };

                    Box::new(iter.filter_map(switch_statement))
                }
                Transducers::Take(num) => generate_take!(iter, num, cur_inst_span),
                Transducers::Drop(num) => generate_drop!(iter, num, cur_inst_span),
            }
        }

        let vm_stack_copy = Rc::clone(&vm_stack);
        let vm_stack_index_copy = Rc::clone(&vm_stack_index);
        let function_stack_copy = Rc::clone(&function_stack);
        let global_env_copy = Rc::clone(&global_env);

        let switch_statement = move |acc, x| match &reducer {
            SteelVal::FuncV(func) => {
                let arg_vec = [acc?, x?];
                func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
            }
            SteelVal::BoxedFunction(func) => {
                let arg_vec = [acc?, x?];
                func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
            }
            SteelVal::ContractedFunction(cf) => {
                let arg_vec = vec![acc?, x?];
                let mut local_upvalue_heap = UpValueHeap::new();
                cf.apply(
                    arg_vec,
                    constants,
                    cur_inst_span,
                    callback,
                    &mut local_upvalue_heap,
                    &mut global_env_copy.borrow_mut(),
                    &mut vm_stack_copy.borrow_mut(),
                    &mut function_stack_copy.borrow_mut(),
                    &mut vm_stack_index_copy.borrow_mut(),
                    use_callbacks,
                    apply_contracts,
                )
            }
            SteelVal::Closure(closure) => {
                // Set the state prior to the recursive call
                vm_stack_index.borrow_mut().push(vm_stack.borrow().len());

                vm_stack.borrow_mut().push(acc?);
                vm_stack.borrow_mut().push(x?);

                function_stack.borrow_mut().push(Gc::clone(closure));

                vm(
                    closure.body_exp(),
                    &mut vm_stack.borrow_mut(),
                    &mut global_env_copy.borrow_mut(),
                    constants,
                    callback,
                    &mut heap.borrow_mut(),
                    &mut function_stack.borrow_mut(),
                    &mut vm_stack_index_copy.borrow_mut(),
                    use_callbacks,
                    apply_contracts,
                    None,
                )
            }

            _ => stop!(TypeMismatch => "reduce expected a function"; *cur_inst_span),
        };

        iter.fold(Ok(initial_value), switch_statement)
    }
}
