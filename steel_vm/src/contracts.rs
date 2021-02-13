use crate::evaluation_progress::EvaluationProgress;
use crate::heap::Heap;
use crate::vm::vm;
use std::cell::RefCell;
use std::rc::Rc;
use steel::env::Env;
use steel::gc::Gc;
use steel::new_parser::span::Span;
use steel::rerrs::SteelErr;
use steel::rvals::{Result, SteelVal};
use steel::steel_compiler::constants::ConstantTable;
use steel::stop;

use steel::contracts::{ContractType, ContractedFunction, FlatContract, FunctionContract};

use log::error;

pub trait ContractedFunctionExt {
    fn apply<CT: ConstantTable>(
        &self,
        arguments: Vec<Gc<SteelVal>>,
        local_heap: &mut Heap,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<Gc<SteelVal>>;
}

impl ContractedFunctionExt for ContractedFunction {
    fn apply<CT: ConstantTable>(
        &self,
        arguments: Vec<Gc<SteelVal>>,
        local_heap: &mut Heap,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<Gc<SteelVal>> {
        // unimplemented!()

        // if self.function.arity() != self.contract.arity()

        let mut verified_args = Vec::new();

        for (arg, contract) in arguments
            .iter()
            .map(Gc::clone)
            .into_iter()
            .zip(self.contract.pre_conditions().iter())
        {
            match contract {
                ContractType::Flat(f) => {
                    // unimplemented!();
                    f.apply(
                        Gc::clone(&arg),
                        local_heap,
                        constants,
                        cur_inst_span,
                        repl,
                        callback,
                    )?;
                    verified_args.push(arg);
                }
                ContractType::Function(fc) => match arg.as_ref() {
                    SteelVal::ContractedFunction(contracted_function) => {
                        if &contracted_function.contract != fc {
                            stop!(ContractViolation => "function contract does not match required contract"; *cur_inst_span);
                        } else {
                            verified_args.push(arg)
                        }
                    }
                    SteelVal::Closure(c) => verified_args.push(Gc::new(
                        ContractedFunction::new(fc.clone(), c.clone()).into(),
                    )),
                    _ => {
                        stop!(ContractViolation => "contracts not yet supported with non user defined"; *cur_inst_span)
                    }
                },
            }
        }

        let output = {
            let parent_env = self.function.sub_expression_env();

            // TODO remove this unwrap
            let offset =
                self.function.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

            let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                parent_env.clone(),
                offset,
            )));

            inner_env
                .borrow_mut()
                .reserve_defs(if self.function.ndef_body() > 0 {
                    self.function.ndef_body() - 1
                } else {
                    0
                });

            vm(
                self.function.body_exp(),
                verified_args.into(),
                local_heap,
                inner_env,
                constants,
                repl,
                callback,
            )
        }?;

        match self.contract.post_condition().as_ref() {
            ContractType::Flat(f) => {
                // unimplemented!();

                if let Err(e) = f.apply(
                    Gc::clone(&output),
                    local_heap,
                    constants,
                    cur_inst_span,
                    repl,
                    callback,
                ) {
                    stop!(ContractViolation => format!("error occured in the range position: {}", e.to_string()); *cur_inst_span);
                }

                Ok(output)
            }
            ContractType::Function(fc) => match output.as_ref() {
                SteelVal::ContractedFunction(contracted_function) => {
                    if &contracted_function.contract != fc {
                        stop!(ContractViolation => "function contract does not match required contract"; *cur_inst_span);
                    } else {
                        Ok(output)
                    }
                }
                SteelVal::Closure(c) => Ok(Gc::new(
                    ContractedFunction::new(fc.clone(), c.clone()).into(),
                )),
                _ => {
                    stop!(ContractViolation => "contracts not yet supported with non user defined"; *cur_inst_span)
                }
            },
        }
    }
}

pub trait FlatContractExt {
    fn apply<CT: ConstantTable>(
        &self,
        arg: Gc<SteelVal>,
        local_heap: &mut Heap,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<()>;
}

impl FlatContractExt for FlatContract {
    fn apply<CT: ConstantTable>(
        &self,
        arg: Gc<SteelVal>,
        local_heap: &mut Heap,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<()> {
        let arg_vec = vec![Gc::clone(&arg)];
        let output = match self.predicate().as_ref() {
            SteelVal::FuncV(func) => func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span)),
            SteelVal::StructClosureV(factory, func) => {
                func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span))
            }
            SteelVal::Closure(closure) => {
                let parent_env = closure.sub_expression_env();

                // TODO remove this unwrap
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

                // TODO make recursive call here with a very small stack
                // probably a bit overkill, but not much else I can do here I think
                vm(
                    closure.body_exp(),
                    arg_vec.into(),
                    local_heap,
                    inner_env,
                    constants,
                    repl,
                    callback,
                )
            }
            _ => stop!(TypeMismatch => "contract expected a function"; *cur_inst_span),
        }?;

        if output.as_ref().is_truthy() {
            Ok(())
        } else {
            stop!(ContractViolation => format!("Found in the application of a flat contract for {}: the given input: {} resulted in a contract violation", &self.name, arg); *cur_inst_span);
        }
    }
}

pub trait FunctionContractExt {
    fn apply<CT: ConstantTable>(&self);
}
