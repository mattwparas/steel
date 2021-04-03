use super::contracts::ContractedFunctionExt;
use super::evaluation_progress::EvaluationProgress;
use super::heap::Heap;
use super::heap2::UpValueHeap;
use super::vm::vm;
use crate::compiler::constants::ConstantTable;
use crate::env::Env;
use crate::parser::span::Span;
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::cell::RefCell;
use std::rc::Rc;

/// Function to inline the reducer function
/// Used for the accumulating a result by wrapping the fold iterator
pub(crate) fn inline_reduce_iter<
    'global,
    I: Iterator<Item = Result<SteelVal>> + 'global,
    CT: ConstantTable,
>(
    iter: I,
    initial_value: SteelVal,
    reducer: SteelVal,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
    callback: &'global EvaluationProgress,
) -> Result<SteelVal> {
    // unimplemented!();

    let switch_statement = move |acc, x| match &reducer {
        SteelVal::FuncV(func) => {
            let arg_vec = [acc?, x?];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::BoxedFunction(func) => {
            let arg_vec = [acc?, x?];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        // SteelVal::StructClosureV(sc) => {
        //     let arg_vec = vec![acc?, x?];
        //     (sc.func)(&arg_vec, &sc.factory).map_err(|x| x.set_span(*cur_inst_span))
        // }
        SteelVal::ContractedFunction(cf) => {
            let arg_vec = vec![acc?, x?];
            let mut local_heap = Heap::new();
            let mut local_upvalue_heap = UpValueHeap::new();
            cf.apply(
                arg_vec,
                &mut local_heap,
                constants,
                cur_inst_span,
                repl,
                callback,
                &mut local_upvalue_heap,
            )
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![acc?, x?];
            // if let Some()

            let parent_env = closure.sub_expression_env();

            // TODO remove this unwrap
            // let offset = closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

            let inner_env = Rc::new(RefCell::new(
                Env::new_subexpression_with_capacity_without_offset(parent_env.clone()),
            ));

            // inner_env
            //     .borrow_mut()
            //     .reserve_defs(if closure.ndef_body() > 0 {
            //         closure.ndef_body() - 1
            //     } else {
            //         0
            //     });

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
                repl,
                callback,
                &mut local_upvalue_heap,
            )
        }

        _ => stop!(TypeMismatch => "reduce expected a function"; *cur_inst_span),
    };

    iter.fold(Ok(initial_value), switch_statement)
}

pub(crate) fn inline_map_result_iter<
    'global,
    I: Iterator<Item = Result<SteelVal>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: SteelVal,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
    callback: &'global EvaluationProgress,
) -> impl Iterator<Item = Result<SteelVal>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
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
                repl,
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
            let offset = closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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
                repl,
                callback,
                &mut local_upvalue_heap,
            )
        }
        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
    };

    iter.map(switch_statement)
}

pub(crate) fn inline_filter_result_iter<
    'global,
    I: Iterator<Item = Result<SteelVal>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: SteelVal,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
    callback: &'global EvaluationProgress,
) -> impl Iterator<Item = Result<SteelVal>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg: Result<SteelVal>| match arg {
        Ok(arg) => {
            match &stack_func {
                SteelVal::FuncV(func) => {
                    let arg_vec = [arg.clone()];
                    let res = func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span));
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
                    let res = func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span));
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
                        repl,
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

                    let offset =
                        closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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
                    let res = vm(
                        closure.body_exp(),
                        args.into(),
                        &mut local_heap,
                        inner_env,
                        constants,
                        repl,
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
                _ => Some(Err(SteelErr::new(
                    ErrorKind::TypeMismatch,
                    "map expected a function".to_string(),
                )
                .with_span(*cur_inst_span))),
            }
        }

        _ => Some(arg),
    };

    iter.filter_map(switch_statement)
}
