use super::vm::VmCore;
use crate::{
    gc::Gc,
    parser::span::Span,
    rvals::{Result, SteelVal},
    stop,
    values::contracts::{
        Contract, ContractType, ContractedFunction, DependentContract, FlatContract,
        FunctionContract, FunctionKind,
    },
};

use log::debug;

impl ContractedFunction {
    pub fn apply<'a>(
        &self,
        arguments: Vec<SteelVal>,
        cur_inst_span: &Span,
        ctx: &mut VmCore<'a>,
    ) -> Result<SteelVal> {
        // Walk back and find the contracts to apply
        {
            let mut parent = self.contract.parent();
            while let Some(p) = parent {
                println!("Applying parents");
                p.apply(&self.name, &self.function, &arguments, cur_inst_span, ctx)?;

                parent = p.parent()
            }
        }

        self.contract
            .apply(&self.name, &self.function, &arguments, cur_inst_span, ctx)
    }
}

impl FlatContract {
    pub fn apply<'a>(
        &self,
        arg: SteelVal,
        cur_inst_span: &Span,
        ctx: &mut VmCore<'a>,
    ) -> Result<()> {
        // TODO make this not clone the argument
        let output = match self.predicate() {
            SteelVal::FuncV(func) => func(&[arg.clone()]).map_err(|x| x.set_span(*cur_inst_span)),
            SteelVal::BoxedFunction(func) => {
                func(&[arg.clone()]).map_err(|x| x.set_span(*cur_inst_span))
            }
            SteelVal::Closure(closure) => ctx.call_with_one_arg(closure, arg.clone()),
            SteelVal::ContractedFunction(c) => c.apply(vec![arg.clone()], cur_inst_span, ctx),
            _ => {
                stop!(TypeMismatch => format!("contract expected a function, found: {:?}", self.predicate()); *cur_inst_span)
            }
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
    fn apply<'a>(
        &self,
        name: &Option<String>,
        // function: &Gc<ByteCodeLambda>,
        function: &SteelVal,
        arguments: &[SteelVal],
        cur_inst_span: &Span,
        ctx: &mut VmCore<'a>,
    ) -> Result<SteelVal>;
}

impl FunctionContractExt for FunctionKind {
    fn apply<'a>(
        &self,
        name: &Option<String>,
        function: &SteelVal,
        arguments: &[SteelVal],
        cur_inst_span: &Span,
        ctx: &mut VmCore<'a>,
    ) -> Result<SteelVal> {
        match self {
            Self::Basic(fc) => fc.apply(name, function, arguments, cur_inst_span, ctx),
            Self::Dependent(dc) => dc.apply(name, function, arguments, cur_inst_span, ctx),
        }
    }
}

impl FunctionContractExt for DependentContract {
    fn apply<'a>(
        &self,
        name: &Option<String>,
        function: &SteelVal,
        arguments: &[SteelVal],
        cur_inst_span: &Span,
        ctx: &mut VmCore<'a>,
    ) -> Result<SteelVal> {
        let mut verified_args: Vec<SteelVal> = Vec::new();

        for (i, (arg, dependent_pair)) in
            arguments.iter().zip(self.pre_conditions.iter()).enumerate()
        {
            let thunk = &dependent_pair.thunk;

            let arg_stack = dependent_pair
                .arguments
                .iter()
                .map(|named_argument| {
                    self.arg_positions
                        .get(named_argument)
                        .and_then(|x| arguments.get(*x))
                        .cloned()
                })
                .collect::<Option<Vec<SteelVal>>>()
                .expect("missing argument in dependent contract");

            let contract = {
                ctx.call_with_args(thunk, arg_stack)?.contract_or_else(
                    throw!(TypeMismatch => "dependent contract expected a contract"),
                )?
            };

            match contract.as_ref() {
                ContractType::Flat(f) => {
                    debug!("applying flat contract in pre condition: {}", f.name);

                    if let Err(e) = f.apply(arg.clone(), cur_inst_span, ctx) {
                        debug!(
                            "Blame locations: {:?}, {:?}",
                            self.contract_attachment_location, name
                        );

                        let message = format!("This function call caused an error - it occured in the domain position: {}, with the contract: {}, {}, blaming: {:?} (callsite)", i, self, e, self.contract_attachment_location);

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
                },
            }
        }

        let output = match function {
            SteelVal::Closure(function) => ctx.call_with_args(function, verified_args)?,
            SteelVal::BoxedFunction(f) => {
                f(&verified_args).map_err(|x| x.set_span(*cur_inst_span))?
            }
            SteelVal::FuncV(f) => f(&verified_args).map_err(|x| x.set_span(*cur_inst_span))?,
            SteelVal::FutureFunc(f) => SteelVal::FutureV(Gc::new(
                f(&verified_args).map_err(|x| x.set_span(*cur_inst_span))?,
            )),
            _ => {
                todo!("Implement contract application for non bytecode values");
            }
        };

        let thunk = &self.post_condition.thunk;

        let arg_stack = self
            .post_condition
            .arguments
            .iter()
            .map(|named_argument| {
                self.arg_positions
                    .get(named_argument)
                    .and_then(|x| arguments.get(*x))
                    .cloned()
            })
            .collect::<Option<Vec<SteelVal>>>()
            .expect("missing argument in dependent contract");

        let contract = {
            ctx.call_with_args(thunk, arg_stack)?.contract_or_else(
                throw!(TypeMismatch => "dependent contract expected a contract"),
            )?
        };

        match contract.as_ref() {
            ContractType::Flat(f) => {
                debug!("applying flat contract in post condition: {}", f.name);

                if let Err(e) = f.apply(output.clone(), cur_inst_span, ctx) {
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
                        let error_message = format!("this function call resulted in an error - occured in the range position of this contract: {self} \n
                        {e}
                        blaming: {blame_location} - broke its own contract");

                        stop!(ContractViolation => error_message; *cur_inst_span);
                    } else {
                        let error_message = format!("this function call resulted in an error - occured in the range position of this contract: {self} \n
                        {e}
                        blaming: None - broke its own contract");

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

                _ => Ok(ContractedFunction::new(fc.clone(), output, name.clone()).into()),
            },
        }
    }
}

impl FunctionContract {
    pub fn apply<'a>(
        &self,
        name: &Option<String>,
        function: &SteelVal,
        arguments: &[SteelVal],
        cur_inst_span: &Span,
        ctx: &mut VmCore<'a>,
    ) -> Result<SteelVal> {
        let verified_args = self.verify_preconditions(arguments, cur_inst_span, ctx, name)?;

        // TODO use actual VM with real stack instead

        /*
            Ideas for this: call a builtin

        */

        let output = match function {
            SteelVal::Closure(function) => {
                // TODO: Here is the problem - we recur by calling the function
                // What we should do is actually leverage the stack in the VM directly instead of making a recursive call here
                ctx.call_with_args(function, verified_args.into_iter())?
            }
            SteelVal::BoxedFunction(f) => {
                f(&verified_args).map_err(|x| x.set_span(*cur_inst_span))?
            }
            SteelVal::FuncV(f) => f(&verified_args).map_err(|x| x.set_span(*cur_inst_span))?,
            SteelVal::FutureFunc(f) => SteelVal::FutureV(Gc::new(
                f(&verified_args).map_err(|x| x.set_span(*cur_inst_span))?,
            )),
            _ => {
                todo!("Implement contract application for non bytecode values");
            }
        };

        match self.post_condition().as_ref() {
            ContractType::Flat(f) => {
                // unimplemented!();

                debug!("applying flat contract in post condition: {}", f.name);

                if let Err(e) = f.apply(output.clone(), cur_inst_span, ctx) {
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
                        let error_message = format!("this function call resulted in an error - occured in the range position of this contract: {self} \n
                        {e}
                        blaming: {blame_location} - broke its own contract");

                        stop!(ContractViolation => error_message; *cur_inst_span);
                    } else {
                        let error_message = format!("this function call resulted in an error - occured in the range position of this contract: {self} \n
                        {e}
                        blaming: None - broke its own contract");

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

                _ => Ok(ContractedFunction::new(fc.clone(), output, name.clone()).into()),
            },
        }
    }
}

impl FunctionContract {
    fn verify_preconditions(
        &self,
        arguments: &[SteelVal],
        cur_inst_span: &Span,
        ctx: &mut VmCore,
        name: &Option<String>,
    ) -> Result<Vec<SteelVal>> {
        let mut verified_args = Vec::new();

        for (i, (arg, contract)) in arguments
            .iter()
            .zip(self.pre_conditions().iter())
            .enumerate()
        {
            match contract.as_ref() {
                ContractType::Flat(f) => {
                    debug!("applying flat contract in pre condition: {}", f.name);

                    if let Err(e) = f.apply(arg.clone(), cur_inst_span, ctx) {
                        debug!(
                            "Blame locations: {:?}, {:?}",
                            self.contract_attachment_location, name
                        );

                        let message = format!("This function call caused an error - it occured in the domain position: {}, with the contract: {}, {}, blaming: {:?} (callsite)", i, self, e, self.contract_attachment_location);

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
                },
            }
        }

        Ok(verified_args)
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

    // TODO: This will fail on old contract implementation
    // #[test]
    // fn tail_call_mutual_recursion() {
    //     let script = r#"
    //     (define/contract (foo x)
    //       (->/c int? int?)
    //         (if (= x 100)
    //             x
    //             (bar (+ x 1))))

    //     (define/contract (bar x)
    //       (->/c int? int?)
    //         (if (= x 100)
    //             x
    //             (foo (+ x 1))))

    //     (assert! (equal? (foo 0) 100))
    //   "#;
    //     assert_script(script);
    // }

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

        (define res (transduce (range 0 10) (taking 5) (into-reducer + 0)))

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

        (define exec-list (transduce (range 0 100) xyz (into-list)))
        (define exec-vector (transduce (range 0 100) xyz (into-vector)))
        (define my-sum (transduce (range 0 100) xyz (into-reducer + 0)))
        
        (assert! (equal? exec-list '(2 4 6 8 10 12 14 16 18 20)))
        (assert! (equal? exec-vector (vector 2 4 6 8 10 12 14 16 18 20)))
        (assert! (equal? my-sum 110))
        "#;
        assert_script(script);
    }
}
