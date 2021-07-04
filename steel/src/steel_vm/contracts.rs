use super::{
    evaluation_progress::EvaluationProgress,
    heap::UpValueHeap,
    options::{ApplyContracts, UseCallbacks},
    stack::StackFrame,
    vm::vm,
};
use crate::{
    compiler::constants::ConstantTable,
    env::Env,
    gc::Gc,
    parser::span::Span,
    rerrs::{ErrorKind, SteelErr},
    rvals::{ByteCodeLambda, Result, SteelVal},
    stop,
    values::contracts::{ContractType, ContractedFunction, FlatContract, FunctionContract},
};

use log::debug;

use super::stack::Stack;

// let vm_stack = Rc::new(RefCell::new(&mut self.stack));
// let vm_stack_index = Rc::new(RefCell::new(&mut self.stack_index));
// let function_stack = Rc::new(RefCell::new(&mut self.function_stack));

/// Extension trait for the application of contracted functions
pub(crate) trait ContractedFunctionExt {
    fn apply<CT: ConstantTable, U: UseCallbacks, A: ApplyContracts>(
        &self,
        arguments: Vec<SteelVal>,
        constants: &CT,
        cur_inst_span: &Span,
        callback: &EvaluationProgress,
        upvalue_heap: &mut UpValueHeap,
        global_env: &mut Env,
        stack: &mut StackFrame,
        function_stack: &mut Vec<Gc<ByteCodeLambda>>,
        stack_index: &mut Stack<usize>,
        use_callbacks: U,
        apply_contracts: A,
    ) -> Result<SteelVal>;
}

impl ContractedFunctionExt for ContractedFunction {
    fn apply<CT: ConstantTable, U: UseCallbacks, A: ApplyContracts>(
        &self,
        arguments: Vec<SteelVal>,
        constants: &CT,
        cur_inst_span: &Span,
        callback: &EvaluationProgress,
        upvalue_heap: &mut UpValueHeap,
        global_env: &mut Env,
        stack: &mut StackFrame,
        function_stack: &mut Vec<Gc<ByteCodeLambda>>,
        stack_index: &mut Stack<usize>,
        use_callbacks: U,
        apply_contracts: A,
    ) -> Result<SteelVal> {
        // Walk back and find the contracts to apply
        {
            let mut parent = self.contract.parent();
            while let Some(p) = parent {
                p.apply(
                    &self.name,
                    &self.function,
                    &arguments,
                    constants,
                    cur_inst_span,
                    callback,
                    upvalue_heap,
                    global_env,
                    stack,
                    function_stack,
                    stack_index,
                    use_callbacks,
                    apply_contracts,
                )?;

                parent = p.parent()
            }
        }

        self.contract.apply(
            &self.name,
            &self.function,
            &arguments,
            constants,
            cur_inst_span,
            callback,
            upvalue_heap,
            global_env,
            stack,
            function_stack,
            stack_index,
            use_callbacks,
            apply_contracts,
        )
    }
}

/// Extension trait for the application of flat contracts
pub(crate) trait FlatContractExt {
    fn apply<CT: ConstantTable, U: UseCallbacks, A: ApplyContracts>(
        &self,
        arg: SteelVal,
        constants: &CT,
        cur_inst_span: &Span,
        callback: &EvaluationProgress,
        upvalue_heap: &mut UpValueHeap,
        global_env: &mut Env,
        stack: &mut StackFrame,
        function_stack: &mut Vec<Gc<ByteCodeLambda>>,
        stack_index: &mut Stack<usize>,
        use_callbacks: U,
        apply_contracts: A,
    ) -> Result<()>;
}

impl FlatContractExt for FlatContract {
    fn apply<CT: ConstantTable, U: UseCallbacks, A: ApplyContracts>(
        &self,
        arg: SteelVal,
        constants: &CT,
        cur_inst_span: &Span,
        callback: &EvaluationProgress,
        upvalue_heap: &mut UpValueHeap,
        global_env: &mut Env,
        stack: &mut StackFrame,
        function_stack: &mut Vec<Gc<ByteCodeLambda>>,
        stack_index: &mut Stack<usize>,
        use_callbacks: U,
        apply_contracts: A,
    ) -> Result<()> {
        // TODO make this not clone the argument
        let output = match self.predicate() {
            SteelVal::FuncV(func) => func(&[arg.clone()]).map_err(|x| x.set_span(*cur_inst_span)),
            SteelVal::BoxedFunction(func) => {
                func(&[arg.clone()]).map_err(|x| x.set_span(*cur_inst_span))
            }
            SteelVal::Closure(closure) => {
                stack_index.push(stack.len());
                stack.push(arg.clone());
                function_stack.push(Gc::clone(closure));

                vm(
                    closure.body_exp(),
                    stack,
                    global_env,
                    constants,
                    callback,
                    upvalue_heap,
                    function_stack,
                    stack_index,
                    use_callbacks,
                    apply_contracts,
                    None,
                )
            }
            _ => stop!(TypeMismatch => "contract expected a function"; *cur_inst_span),
        }?;

        if output.is_truthy() {
            Ok(())
        } else {
            stop!(ContractViolation => format!("Found in the application of a flat contract for {}: the given input: {} resulted in a contract violation", &self.name, arg); *cur_inst_span);
        }
    }
}

/// Extension trait for the application of function contracts
pub(crate) trait FunctionContractExt {
    fn apply<CT: ConstantTable, U: UseCallbacks, A: ApplyContracts>(
        &self,
        name: &Option<String>,
        // function: &Gc<ByteCodeLambda>,
        function: &SteelVal,
        arguments: &[SteelVal],
        constants: &CT,
        cur_inst_span: &Span,
        callback: &EvaluationProgress,
        upvalue_heap: &mut UpValueHeap,
        global_env: &mut Env,
        stack: &mut StackFrame,
        function_stack: &mut Vec<Gc<ByteCodeLambda>>,
        stack_index: &mut Stack<usize>,
        use_callbacks: U,
        apply_contracts: A,
    ) -> Result<SteelVal>;
}

impl FunctionContractExt for FunctionContract {
    fn apply<CT: ConstantTable, U: UseCallbacks, A: ApplyContracts>(
        &self,
        name: &Option<String>,
        // function: &Gc<ByteCodeLambda>,
        function: &SteelVal,
        arguments: &[SteelVal],
        constants: &CT,
        cur_inst_span: &Span,
        callback: &EvaluationProgress,
        upvalue_heap: &mut UpValueHeap,
        global_env: &mut Env,
        stack: &mut StackFrame,
        function_stack: &mut Vec<Gc<ByteCodeLambda>>,
        stack_index: &mut Stack<usize>,
        use_callbacks: U,
        apply_contracts: A,
    ) -> Result<SteelVal> {
        let mut verified_args = Vec::new();

        for (i, (arg, contract)) in arguments
            .iter()
            .zip(self.pre_conditions().iter())
            .enumerate()
        {
            match contract.as_ref() {
                ContractType::Flat(f) => {
                    debug!("applying flat contract in pre condition: {}", f.name);
                    // unimplemented!();

                    if let Err(e) = f.apply(
                        arg.clone(),
                        constants,
                        cur_inst_span,
                        callback,
                        upvalue_heap,
                        global_env,
                        stack,
                        function_stack,
                        stack_index,
                        use_callbacks,
                        apply_contracts,
                    ) {
                        debug!(
                            "Blame locations: {:?}, {:?}",
                            self.contract_attachment_location, name
                        );

                        let message = format!("This function call caused an error - it occured in the domain position: {}, with the contract: {}, {}, blaming: {:?} (callsite)", i, self.to_string(), e.to_string(), self.contract_attachment_location);

                        stop!(ContractViolation => message; *cur_inst_span);
                    }

                    verified_args.push(arg.clone());
                }
                ContractType::Function(fc) => match arg {
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
                        let new_arg = ContractedFunction::new(fc, func, name.clone()).into();

                        verified_args.push(new_arg);
                    }

                    _ => verified_args.push(
                        ContractedFunction::new(fc.clone(), arg.clone(), name.clone()).into(),
                    ),
                    // TODO fix name, don't pass in None
                    // SteelVal::Closure(c) => verified_args
                    //     .push(ContractedFunction::new(fc.clone(), c.clone(), name.clone()).into()),
                    // _ => {
                    //     stop!(ContractViolation => "contracts not yet supported with non user defined"; *cur_inst_span)
                    // }
                },
            }
        }

        let output = match function {
            SteelVal::Closure(function) => {
                function_stack.push(Gc::clone(function));

                vm(
                    function.body_exp(),
                    &mut verified_args.into(),
                    global_env, // TODO remove this as well
                    constants,
                    callback,
                    upvalue_heap,
                    function_stack,
                    &mut Stack::new(),
                    use_callbacks,
                    apply_contracts,
                    None,
                )?
            }
            SteelVal::BoxedFunction(f) => {
                // f(&[local, const_value]).map_err(|x| x.set_span(*span))?)
                // self.ip += 4;
                // todo!()
                f(&verified_args).map_err(|x| x.set_span(*cur_inst_span))?
            }
            SteelVal::FuncV(f) => {
                // self.stack
                // .push(f(&[local, const_value]).map_err(|x| x.set_span(*span))?);
                // self.ip += 4;
                // todo!()

                f(&verified_args).map_err(|x| x.set_span(*cur_inst_span))?
            }
            SteelVal::FutureFunc(f) => {
                // let result = SteelVal::FutureV(Gc::new(
                // f(&[local, const_value]).map_err(|x| x.set_span(*span))?,
                // ));

                // self.stack.push(result);
                // self.ip += 4;
                // todo!()
                SteelVal::FutureV(Gc::new(
                    f(&verified_args).map_err(|x| x.set_span(*cur_inst_span))?,
                ))
            }
            _ => {
                todo!("Implement contract application for non bytecode values");
            }
        };

        match self.post_condition().as_ref() {
            ContractType::Flat(f) => {
                // unimplemented!();

                debug!("applying flat contract in post condition: {}", f.name);

                if let Err(e) = f.apply(
                    output.clone(),
                    constants,
                    cur_inst_span,
                    callback,
                    upvalue_heap,
                    global_env,
                    stack,
                    function_stack,
                    stack_index,
                    use_callbacks,
                    apply_contracts,
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

                    // TODO clean this up
                    if let Some(blame_location) = blame_location {
                        let error_message = format!("this function call resulted in an error - occured in the range position of this contract: {} \n
                        {}
                        blaming: {} - broke its own contract", self.to_string(), e.to_string(), blame_location);

                        stop!(ContractViolation => error_message; *cur_inst_span);
                    } else {
                        let error_message = format!("this function call resulted in an error - occured in the range position of this contract: {} \n
                        {}
                        blaming: None - broke its own contract", self.to_string(), e.to_string());

                        stop!(ContractViolation => error_message; *cur_inst_span);
                    }
                }

                Ok(output)
            }
            ContractType::Function(fc) => match output {
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
                    let output = ContractedFunction::new(fc, func, name.clone()).into();

                    Ok(output)
                }

                // TODO don't pass in None
                // SteelVal::Closure(c) => {
                //     Ok(ContractedFunction::new(fc.clone(), c, name.clone()).into())
                // }
                // _ => {
                //     stop!(ContractViolation => "contracts not yet supported with non user defined"; *cur_inst_span)
                // }
                _ => Ok(ContractedFunction::new(fc.clone(), output, name.clone()).into()),
            },
        }
    }
}

#[cfg(test)]
mod contract_tests {
    use crate::steel_vm::test_util::{assert_script, assert_script_error};

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

    #[test]
    fn contract_with_filter() {
        let script = r#"
        (define/contract (is-even? x)
            (->/c number? boolean?)
            (even? x))

        (define res (filter is-even? (range 0 10)))

        (assert! (equal? res '(0 2 4 6 8)))
        "#;

        assert_script(script);
    }

    #[test]
    fn contract_with_map() {
        let script = r#"
        (define/contract (adding1 x)
            (->/c number? number?)
            (+ x 1))

        (define res (map adding1 (range 0 5)))

        (assert! (equal? res '(1 2 3 4 5)))
        "#;

        assert_script(script);
    }

    #[test]
    fn contract_with_reduce() {
        let script = r#"
        (define/contract (reducer accum elem)
            (->/c number? number? number?)
            (+ accum elem))

        (define res (transduce (taking 5) + 0 (range 0 10)))

        (assert! (equal? res 10))
        "#;
        assert_script(script);
    }

    #[test]
    fn transducers_containing_contracts() {
        let script = r#"
        (define/contract (mapper x)
            (->/c number? number?)
            (+ x 1))

        (define/contract (is-even? x)
            (->/c number? boolean?)
            (even? x))

        (define x (mapping mapper))
        (define y (filtering is-even?))
        (define z (taking 10))

        (define xyz (compose x y z))

        (define exec-list (execute xyz (range 0 100)))
        (define exec-vector (execute xyz (range 0 100) 'vector))
        (define my-sum (transduce xyz + 0 (range 0 100)))
        
        (assert! (equal? exec-list '(2 4 6 8 10 12 14 16 18 20)))
        (assert! (equal? exec-vector (vector 2 4 6 8 10 12 14 16 18 20)))
        (assert! (equal? my-sum 110))
        "#;
        assert_script(script);
    }
}
