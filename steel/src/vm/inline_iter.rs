use crate::env::Env;
use crate::gc::Gc;
use crate::parser::span::Span;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::vm::vm;
use crate::vm::ConstantTable;
use crate::vm::DenseInstruction;
use crate::vm::EvaluationProgress;
use crate::vm::Heap;
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) fn inline_reduce_iter<
    'global,
    I: Iterator<Item = Result<Gc<SteelVal>>> + 'global,
    CT: ConstantTable,
>(
    iter: I,
    initial_value: Gc<SteelVal>,
    reducer: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
    callback: &'global EvaluationProgress,
) -> Result<Gc<SteelVal>> {
    // unimplemented!();

    let switch_statement = move |acc, x| match reducer.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![acc?, x?];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![acc?, x?];
            func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![acc?, x?];
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
            )
        }

        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
    };

    iter.fold(Ok(initial_value), switch_statement)
}

pub(crate) fn inline_map_result_iter<
    'global,
    I: Iterator<Item = Result<Gc<SteelVal>>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
    callback: &'global EvaluationProgress,
) -> impl Iterator<Item = Result<Gc<SteelVal>>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![arg?];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![arg?];
            func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span))
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
            )
        }
        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
    };

    iter.map(switch_statement)
}

pub(crate) fn inline_map_iter<
    'global,
    I: Iterator<Item = Gc<SteelVal>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
    callback: &'global EvaluationProgress,
) -> impl Iterator<Item = Result<Gc<SteelVal>>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![arg];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![arg];
            func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![arg];
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
            )
        }
        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
    };

    iter.map(switch_statement)

    // for val in iter {
    //     collected_results.push(switch_statement(val)?);
    // }

    // Ok(collected_results)
}

pub(crate) fn inline_filter_result_iter<
    'global,
    I: Iterator<Item = Result<Gc<SteelVal>>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
    callback: &'global EvaluationProgress,
) -> impl Iterator<Item = Result<Gc<SteelVal>>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg: Result<Gc<SteelVal>>| match arg {
        Ok(arg) => {
            match stack_func.as_ref() {
                SteelVal::FuncV(func) => {
                    let arg_vec = vec![Gc::clone(&arg)];
                    let res = func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span));
                    match res {
                        Ok(k) => match k.as_ref() {
                            SteelVal::BoolV(true) => Some(Ok(arg)),
                            SteelVal::BoolV(false) => None,
                            _ => None,
                        },
                        Err(e) => Some(Err(e)),
                        // _ => None,
                    }
                }
                SteelVal::StructClosureV(factory, func) => {
                    let arg_vec = vec![Gc::clone(&arg)];
                    let res = func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span));
                    match res {
                        Ok(k) => match k.as_ref() {
                            SteelVal::BoolV(true) => Some(Ok(arg)),
                            SteelVal::BoolV(false) => None,
                            _ => None,
                        },
                        Err(e) => Some(Err(e)),
                    }
                }
                SteelVal::Closure(closure) => {
                    // ignore the stack limit here
                    let args = vec![Gc::clone(&arg)];
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
                    );

                    match res {
                        Ok(k) => match k.as_ref() {
                            SteelVal::BoolV(true) => Some(Ok(arg)),
                            SteelVal::BoolV(false) => None,
                            _ => None,
                        },
                        Err(e) => Some(Err(e)),
                    }
                }
                _ => Some(Err(SteelErr::TypeMismatch(
                    "map expected a function".to_string(),
                    Some(*cur_inst_span),
                ))),
            }
        }

        _ => Some(arg),
    };

    iter.filter_map(switch_statement)

    // for val in iter {
    //     collected_results.push(switch_statement(val)?);
    // }

    // Ok(collected_results)
}

pub(crate) fn inline_filter_iter<
    'global,
    I: Iterator<Item = Gc<SteelVal>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
    callback: &'global EvaluationProgress,
) -> impl Iterator<Item = Result<Gc<SteelVal>>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![Gc::clone(&arg)];
            let res = func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span));
            match res {
                Ok(k) => match k.as_ref() {
                    SteelVal::BoolV(true) => Some(Ok(arg)),
                    SteelVal::BoolV(false) => None,
                    _ => None,
                },
                Err(e) => Some(Err(e)),
                // _ => None,
            }
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![Gc::clone(&arg)];
            let res = func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span));
            match res {
                Ok(k) => match k.as_ref() {
                    SteelVal::BoolV(true) => Some(Ok(arg)),
                    SteelVal::BoolV(false) => None,
                    _ => None,
                },
                Err(e) => Some(Err(e)),
            }
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![Gc::clone(&arg)];
            // if let Some()

            let parent_env = closure.sub_expression_env();

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
            );

            match res {
                Ok(k) => match k.as_ref() {
                    SteelVal::BoolV(true) => Some(Ok(arg)),
                    SteelVal::BoolV(false) => None,
                    _ => None,
                },
                Err(e) => Some(Err(e)),
            }
        }
        _ => Some(Err(SteelErr::TypeMismatch(
            "map expected a function".to_string(),
            Some(*cur_inst_span),
        ))),
    };

    iter.filter_map(switch_statement)
}

pub(crate) fn inline_map_normal<I: Iterator<Item = Gc<SteelVal>>, CT: ConstantTable>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &CT,
    cur_inst: &DenseInstruction,
    repl: bool,
    callback: &EvaluationProgress,
) -> Result<Vec<Gc<SteelVal>>> {
    // unimplemented!();

    let mut collected_results: Vec<Gc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    let switch_statement = |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![arg];
            func(&arg_vec).map_err(|x| x.set_span(cur_inst.span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![arg];
            func(arg_vec, factory).map_err(|x| x.set_span(cur_inst.span))
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![arg];
            // if let Some()

            let parent_env = closure.sub_expression_env();

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
            )
        }
        _ => stop!(TypeMismatch => "map expected a function"; cur_inst.span),
    };

    for val in iter {
        collected_results.push(switch_statement(val)?);
    }

    Ok(collected_results)
}

pub(crate) fn inline_filter_normal<I: Iterator<Item = Gc<SteelVal>>, CT: ConstantTable>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &CT,
    cur_inst: &DenseInstruction,
    repl: bool,
    callback: &EvaluationProgress,
) -> Result<Vec<Gc<SteelVal>>> {
    // unimplemented!();

    let mut collected_results: Vec<Gc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    let switch_statement = |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![arg];
            func(&arg_vec).map_err(|x| x.set_span(cur_inst.span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![arg];
            func(arg_vec, factory).map_err(|x| x.set_span(cur_inst.span))
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![arg];
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
            )
        }
        _ => stop!(TypeMismatch => "filter expected a function"; cur_inst.span),
    };

    for val in iter {
        let res = switch_statement(Gc::clone(&val))?;
        if let SteelVal::BoolV(true) = res.as_ref() {
            collected_results.push(val);
        }
    }

    Ok(collected_results)
}
