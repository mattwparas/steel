use crate::evaluation_progress::EvaluationProgress;
use crate::heap::Heap;
use crate::vm::vm;
use std::cell::RefCell;
use std::rc::Rc;
use steel::gc::Gc;
use steel::parser::span::Span;
use steel::rerrs::SteelErr;
use steel::rvals::{Result, SteelVal};
use steel::steel_compiler::constants::ConstantTable;
use steel::stop;
use steel::{env::Env, rvals::ByteCodeLambda};

use steel::contracts::{ContractType, ContractedFunction, FlatContract, FunctionContract};

use log::debug;

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
        // Walk back and find the contracts to apply
        {
            let mut parent = self.contract.parent();
            while let Some(p) = parent {
                p.apply(
                    &self.name,
                    &self.function,
                    &arguments,
                    local_heap,
                    constants,
                    cur_inst_span,
                    repl,
                    callback,
                )?;

                parent = p.parent()
            }
        }

        self.contract.apply(
            &self.name,
            &self.function,
            &arguments,
            local_heap,
            constants,
            cur_inst_span,
            repl,
            callback,
        )
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
    fn apply<CT: ConstantTable>(
        &self,
        name: &Option<String>,
        function: &ByteCodeLambda,
        arguments: &[Gc<SteelVal>],
        local_heap: &mut Heap,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<Gc<SteelVal>>;
}

impl FunctionContractExt for FunctionContract {
    fn apply<CT: ConstantTable>(
        &self,
        name: &Option<String>,
        function: &ByteCodeLambda,
        arguments: &[Gc<SteelVal>],
        local_heap: &mut Heap,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<Gc<SteelVal>> {
        let mut verified_args = Vec::new();

        for (i, (arg, contract)) in arguments
            .iter()
            .zip(self.pre_conditions().iter())
            .enumerate()
        {
            match contract {
                ContractType::Flat(f) => {
                    debug!("applying flat contract in pre condition: {}", f.name);
                    // unimplemented!();

                    if let Err(e) = f.apply(
                        Gc::clone(&arg),
                        local_heap,
                        constants,
                        cur_inst_span,
                        repl,
                        callback,
                    ) {
                        debug!(
                            "Blame locations: {:?}, {:?}",
                            self.contract_attachment_location, name
                        );

                        stop!(ContractViolation => format!("This function call caused an error - an occured in the domain position: {}, with the contract: {}, {}, blaming: {:?} (callsite)", i, self.to_string(), e.to_string(), self.contract_attachment_location); *cur_inst_span);
                    }

                    verified_args.push(Gc::clone(arg));
                }
                ContractType::Function(fc) => match arg.as_ref() {
                    SteelVal::ContractedFunction(contracted_function) => {
                        let mut pre_parent = contracted_function.contract.clone();
                        pre_parent.set_attachment_location(contracted_function.name.clone());

                        let parent = Gc::new(pre_parent);

                        let func = contracted_function.function.clone();

                        debug!(
                            "Setting the parent: {} on a precondition function: {}",
                            parent.to_string(),
                            fc.to_string()
                        );

                        // Get the contract down from the
                        let mut fc = fc.clone();
                        fc.set_parent(parent);
                        debug!(
                            "Inside: {:?}, Setting attachment location in range to: {:?}",
                            name, contracted_function.name
                        );
                        fc.set_attachment_location(contracted_function.name.clone());

                        // TODO Don't pass in None
                        let new_arg =
                            Gc::new(ContractedFunction::new(fc, func, name.clone()).into());

                        verified_args.push(new_arg);
                    }

                    // TODO fix name, don't pass in None
                    SteelVal::Closure(c) => verified_args.push(Gc::new(
                        ContractedFunction::new(fc.clone(), c.clone(), name.clone()).into(),
                    )),
                    _ => {
                        stop!(ContractViolation => "contracts not yet supported with non user defined"; *cur_inst_span)
                    }
                },
            }
        }

        let output = {
            let parent_env = function.sub_expression_env();

            // TODO remove this unwrap
            let offset = function.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

            let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                parent_env.clone(),
                offset,
            )));

            inner_env
                .borrow_mut()
                .reserve_defs(if function.ndef_body() > 0 {
                    function.ndef_body() - 1
                } else {
                    0
                });

            vm(
                function.body_exp(),
                verified_args.into(),
                local_heap,
                inner_env,
                constants,
                repl,
                callback,
            )
        }?;

        match self.post_condition().as_ref() {
            ContractType::Flat(f) => {
                // unimplemented!();

                debug!("applying flat contract in post condition: {}", f.name);

                if let Err(e) = f.apply(
                    Gc::clone(&output),
                    local_heap,
                    constants,
                    cur_inst_span,
                    repl,
                    callback,
                ) {
                    debug!(
                        "Blame locations: {:?}, {:?}",
                        self.contract_attachment_location, name
                    );

                    debug!("Parent exists: {}", self.parent().is_some());

                    let blame_location = if self.contract_attachment_location.is_none() {
                        name
                    } else {
                        &self.contract_attachment_location
                    };

                    let error_message = format!("this function call resulted in an error - occured in the range position of this contract: {} \n
                    {}
                    blaming: {:?} - broke its own contract", self.to_string(), e.to_string(), blame_location);

                    stop!(ContractViolation => error_message; *cur_inst_span);
                }

                Ok(output)
            }
            ContractType::Function(fc) => match output.as_ref() {
                SteelVal::ContractedFunction(contracted_function) => {
                    let mut pre_parent = contracted_function.contract.clone();
                    pre_parent.set_attachment_location(contracted_function.name.clone());

                    let parent = Gc::new(pre_parent);

                    let func = contracted_function.function.clone();

                    debug!(
                        "Setting the parent: {} on a postcondition function: {}",
                        parent.to_string(),
                        fc.to_string()
                    );

                    // Get the contract down from the parent
                    let mut fc = fc.clone();
                    fc.set_parent(parent);
                    debug!(
                        "Inside: {:?}, Setting attachment location in range to: {:?}",
                        name, contracted_function.name
                    );

                    // TODO Don't pass in None here
                    let output = Gc::new(ContractedFunction::new(fc, func, name.clone()).into());

                    Ok(output)
                }

                // TODO don't pass in None
                SteelVal::Closure(c) => Ok(Gc::new(
                    ContractedFunction::new(fc.clone(), c.clone(), name.clone()).into(),
                )),
                _ => {
                    stop!(ContractViolation => "contracts not yet supported with non user defined"; *cur_inst_span)
                }
            },
        }
    }
}

#[cfg(test)]
mod contract_tests {
    use crate::test_util::{assert_script, assert_script_error};

    #[test]
    fn simple_flat_contract() {
        let script = r#"
          (define/contract (test x y)
            (->/c even? even? odd?)
            (+ x y 1))

          (assert! (equal? (test 2 2) 5))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_flat_contract_domain_violation() {
        let script = r#"
          (define/contract (test x y)
            (->/c even? even? odd?)
            (+ x y 1))

          (test 1 2)
        "#;
        assert_script_error(script);
    }

    #[test]
    fn simple_higher_order_contract() {
        let script = r#"
          (define/contract (blagh func y)
            (->/c (->/c even? odd?) even? even?)
            (+ 1 (func y)))
            
          (assert! (equal? (blagh (lambda (x) (+ x 1)) 2) 4))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_higher_order_contract_violation() {
        let script = r#"
          (define/contract (blagh func y)
            (->/c (->/c even? odd?) even? even?)
            (+ 1 (func y)))

          (blagh (lambda (x) (+ x 2)) 2)
        "#;
        assert_script_error(script);
    }

    #[test]
    fn tail_call_contract_still_works() {
        let script = r#"
          (define/contract (loop x)
            (->/c int? int?)
              (if (= x 100)
                  x
                  (loop (+ x 1))))

          (assert! (equal? (loop 0) 100))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_checking_on_application_success() {
        let script = r#"

        (define/contract (output)
            (->/c (->/c string? int?))
            (lambda (x) 10))

        (define/contract (accept func)
            (->/c (->/c string? int?) string?)
            "cool cool cool")

        (assert! (equal? (accept (output)) "cool cool cool"))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_checking_not_called_since_not_applied() {
        let script = r#"

        (define/contract (output)
            (->/c (->/c string? int?))
            (lambda (x) 10))

        (define/contract (accept func)
            (->/c (->/c string? string?) string?)
            "cool cool cool")

        (assert! (equal? (accept (output)) "cool cool cool"))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_checking_on_return_does_not_happen() {
        let script = r#"

        (define/contract (output)
            (->/c (->/c string? int?))
            (lambda (x) 10))

        (define/contract (accept)
            (->/c (->/c string? string?))
            (output))

        (accept)
        "#;

        assert_script(script);
    }

    #[test]
    fn contract_application_in_map() {
        let script = r#"
        (define (test x)
            (->/c int? int?)
            (+ x 1))
        (define result (map test (list 1 2 3 4)))
        (assert! (equal? result (list 2 3 4 5)))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_application_in_filter() {
        let script = r#"
        (define (test x)
            (->/c int? boolean?)
            (even? x))
        (define result (filter test (list 1 2 3 4)))
        (assert! (equal? result (list 2 4)))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_checking_with_weaker() {
        let script = r#"
        (define/contract (output)
            (->/c (->/c string? int?))
            (lambda (x) 10.0))

        (define/contract (accept)
            (->/c (->/c string? number?))
            (output))

        ((accept) "test")
        "#;

        assert_script_error(script)
    }

    #[test]
    fn contract_checking_pre_condition_later() {
        let script = r#"
        (define/contract (output)
            (->/c (->/c list? int?))
            (lambda (x) 10.0))

        (define/contract (accept)
            (->/c (->/c string? number?))
            (output))

        ((accept) "test")
        "#;

        assert_script_error(script);
    }

    #[test]
    fn three_levels_of_contracts() {
        let script = r#"
        (define (any? x) #t)

        (define/contract (level1)
            (->/c (->/c int?))
            (lambda () 10.2))

        (define/contract (level2)
            (->/c (->/c number?))
            (level1))

        (define/contract (level3)
            (->/c (->/c any?))
            (level2))

        ((level3))
        "#;

        assert_script_error(script);
    }
}
