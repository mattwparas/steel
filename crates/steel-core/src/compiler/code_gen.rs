use crate::{
    compiler::passes::analysis::IdentifierStatus::{
        Captured, Free, Global, HeapAllocated, LetVar, Local, LocallyDefinedFunction,
    },
    core::{
        instructions::Instruction,
        labels::{resolve_labels, LabeledInstruction},
        opcode::OpCode,
    },
    parser::{
        ast::{Atom, ExprKind, List},
        parser::SyntaxObject,
        span_visitor::get_span,
        tokens::TokenType,
        visitors::VisitorMut,
    },
    stop, SteelVal,
};

use super::{
    constants::ConstantMap,
    passes::analysis::{
        Analysis,
        CallKind::{Normal, SelfTailCall, TailCall},
    },
};

use crate::rvals::Result;

pub struct CodeGenerator<'a> {
    pub(crate) instructions: Vec<LabeledInstruction>,
    constant_map: &'a mut ConstantMap,
    analysis: &'a Analysis,
    local_count: Vec<usize>,
}

fn eval_atom(t: &SyntaxObject) -> Result<SteelVal> {
    match &t.ty {
        TokenType::BooleanLiteral(b) => Ok((*b).into()),
        // TokenType::Identifier(s) => env.borrow().lookup(&s),
        TokenType::NumberLiteral(n) => Ok(SteelVal::NumV(*n)),
        TokenType::StringLiteral(s) => Ok(SteelVal::StringV(s.into())),
        TokenType::CharacterLiteral(c) => Ok(SteelVal::CharV(*c)),
        TokenType::IntegerLiteral(n) => Ok(SteelVal::IntV(*n)),
        // TODO: Keywords shouldn't be misused as an expression - only in function calls are keywords allowed
        TokenType::Keyword(k) => Ok(SteelVal::SymbolV(k.clone().into())),
        what => {
            // println!("getting here in the eval_atom - code_gen");
            stop!(UnexpectedToken => what; t.span)
        }
    }
}

impl<'a> CodeGenerator<'a> {
    pub fn new(constant_map: &'a mut ConstantMap, analysis: &'a Analysis) -> Self {
        CodeGenerator {
            instructions: Vec::new(),
            constant_map,
            analysis,
            local_count: Vec::new(),
        }
    }

    pub fn top_level_compile(mut self, expr: &ExprKind) -> Result<Vec<Instruction>> {
        self.visit(expr)?;
        self.instructions
            .push(LabeledInstruction::builder(OpCode::POPPURE));

        Ok(resolve_labels(self.instructions))
    }

    fn push(&mut self, instr: LabeledInstruction) {
        self.instructions.push(instr);
    }

    fn len(&self) -> usize {
        self.instructions.len()
    }

    fn specialize_constant(&mut self, syn: &SyntaxObject) -> Result<()> {
        let value = eval_atom(syn)?;

        let idx = self.constant_map.add_or_get(value);
        self.push(
            LabeledInstruction::builder(OpCode::PUSHCONST)
                .payload(idx)
                .contents(syn.clone())
                .constant(true),
        );
        Ok(())
    }

    #[allow(unused)]
    fn should_specialize_call(&self, l: &List) -> Option<OpCode> {
        if l.args.len() == 3 {
            let function = l.first()?;

            let _ = l.args[1].atom_identifier()?;
            let _ = eval_atom(l.args[2].atom_syntax_object()?).ok()?;

            if let Some(info) = self.analysis.get(function.atom_syntax_object()?) {
                if info.kind == Free || info.kind == Global {
                    return match function.atom_identifier().unwrap() {
                        "+" => Some(OpCode::ADDREGISTER),
                        "-" => Some(OpCode::SUBREGISTER),
                        "<=" => Some(OpCode::LTEREGISTER),
                        _ => None,
                    };
                }
            }
        }

        None
    }

    #[allow(unused)]
    fn specialize_call(&mut self, l: &List, op: OpCode) -> Result<()> {
        let value = eval_atom(l.args[2].atom_syntax_object().unwrap())?;

        // Specialize SUB1 -> specializing here is a bit much but it should help
        // if value == SteelVal::IntV(1) && op == OpCode::SUBREGISTER {
        //     self.push(LabeledInstruction::builder(OpCode::SUBREGISTER1));

        //     if let Some(analysis) = &l.args[1]
        //         .atom_syntax_object()
        //         .and_then(|a| self.analysis.get(&a))
        //     {
        //         self.push(
        //             LabeledInstruction::builder(OpCode::PASS)
        //                 .payload(analysis.stack_offset.unwrap()),
        //         );
        //     } else {
        //         panic!("Shouldn't be happening")
        //     }

        //     return Ok(());
        // }

        if let Some(analysis) = &l.args[1]
            .atom_syntax_object()
            .and_then(|a| self.analysis.get(a))
        {
            self.push(LabeledInstruction::builder(op).payload(analysis.stack_offset.unwrap()));
        } else {
            panic!("Shouldn't be happening")
        }

        // local variable, map to index:
        // if let ExprKind::Atom(a) = &l.args[1] {
        //     if let Some(analysis) = self.analysis.get(&a.syn) {
        //         self.push(
        //             LabeledInstruction::builder(OpCode::PASS)
        //                 .payload(analysis.stack_offset.unwrap()),
        //         );
        //     } else {
        //         panic!("Shouldn't be getting here")
        //     }
        // } else {
        //     panic!("Shouldn't be getting here")
        // }

        let idx = self.constant_map.add_or_get(value);

        self.push(LabeledInstruction::builder(OpCode::PASS).payload(idx));

        Ok(())
    }
}

impl<'a> VisitorMut for CodeGenerator<'a> {
    type Output = Result<()>;

    // TODO come back later and resolve this properly using labels
    // If looks like label resolution needs to be able to arbitrarily point to a label one
    // instruction after
    fn visit_if(&mut self, f: &crate::parser::ast::If) -> Self::Output {
        // load in the test condition
        self.visit(&f.test_expr)?;
        // Get the if index
        let if_idx = self.instructions.len();
        // push in if
        self.push(LabeledInstruction::builder(OpCode::IF).payload(self.instructions.len() + 2));
        // save spot of jump instruction, fill in after
        // let idx = self.len();
        // self.push(Instruction::new_jmp(0)); // dummy value

        // emit instructions for then
        self.visit(&f.then_expr)?;
        self.push(LabeledInstruction::builder(OpCode::JMP));
        let false_start = self.len();

        // emit instructions for else expression
        self.visit(&f.else_expr)?;
        let j3 = self.len(); // first instruction after else

        // println!("false_start: {:?}", false_start);
        // println!("j3: {:?}", j3);

        // // set index of jump instruction
        // if let Some(elem) = self.instructions.get_mut(idx) {
        //     (*elem).payload_size = false_start;
        // } else {
        //     stop!(Generic => "out of bounds jump");
        // }

        if let Some(elem) = self.instructions.get_mut(false_start - 1) {
            elem.payload_size = j3;
            // (*elem).payload_size = false_start;
        } else {
            stop!(Generic => "out of bounds jump");
        }

        if let Some(elem) = self.instructions.get_mut(if_idx) {
            elem.payload_size = false_start;
            // (*elem).payload_size = false_start;
        } else {
            stop!(Generic => "out of bounds jump");
        }

        Ok(())

        // self.visit(&f.test_expr)?;

        // let if_idx = self.instructions.len();

        // self.push(LabeledInstruction::builder(OpCode::IF).payload(self.instructions.len() + 2));

        // self.visit(&f.then_expr)?;

        // let false_start_label = fresh();
        // let j3_label = fresh();

        // self.push(LabeledInstruction::builder(OpCode::JMP).goto(j3_label));

        // let false_start = self.len(); // index after the jump

        // // self.instructions
        // //     .last_mut()
        // //     .unwrap()
        // //     .set_tag(false_start_label);

        // self.visit(&f.else_expr)?;

        // self.instructions.last_mut().unwrap().set_tag(j3_label);

        // // if let Some(elem) = self.instructions.get_mut(false_start - 1) {
        // //     // (*elem).goto = Some(j3);

        // //     elem.set_goto(j3_label);

        // //     // (*elem).payload_size = false_start;
        // // } else {
        // //     stop!(Generic => "out of bounds jump");
        // // }

        // if let Some(elem) = self.instructions.get_mut(if_idx) {
        //     // (*elem).goto = Some(false_start_label);

        //     elem.set_goto(false_start_label);

        //     // (*elem).payload_size = false_start;
        // } else {
        //     stop!(Generic => "out of bounds jump");
        // }

        // Ok(())
    }

    fn visit_define(&mut self, define: &crate::parser::ast::Define) -> Self::Output {
        // let sidx = self.len();

        if let ExprKind::Atom(name) = &define.name {
            self.push(LabeledInstruction::builder(OpCode::SDEF).contents(name.syn.clone()));

            self.visit(&define.body)?;

            // let defn_body_size = self.len() - sidx;

            // TODO: Consider whether SDEF and EDEF are even necessary at all
            // Just remove them otherwise
            self.push(LabeledInstruction::builder(OpCode::EDEF));

            // if let Some(elem) = self.instructions.get_mut(sidx) {
            //     (*elem).payload_size = defn_body_size;
            // } else {
            //     stop!(Generic => "out of bounds closure len");
            // }

            // println!("binding global: {}", name);
            self.push(LabeledInstruction::builder(OpCode::BIND).contents(name.syn.clone()));

            self.push(LabeledInstruction::builder(OpCode::VOID));
        } else {
            panic!(
                "Complex defines not supported in bytecode generation: {}",
                define.name
            )
        }

        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &crate::parser::ast::LambdaFunction,
    ) -> Self::Output {
        let idx = self.len();

        // Grab the function information from the analysis - this is going to tell us what the captured
        // vars are, and subsequently how to compile the let for this use case
        let function_info = self
            .analysis
            .function_info
            .get(&lambda_function.syntax_object_id)
            .unwrap();

        // Distinguishing between a pure function and a non pure function will enable
        // thinner code for the resulting function
        let op_code = if function_info.captured_vars().is_empty() {
            OpCode::PUREFUNC
        } else {
            OpCode::NEWSCLOSURE
        };

        // Attach the debug symbols here
        self.push(LabeledInstruction::builder(op_code).contents(lambda_function.location.clone()));
        self.push(
            LabeledInstruction::builder(OpCode::PASS).payload(usize::from(lambda_function.rest)),
        );

        let arity = lambda_function.args.len();

        // Patching over the changes, see old code generator for more information
        // TODO: This is not a portable change. Syntax Object IDs need to have a patched unique ID
        // This should be doable by supplementing everything with an offset when consuming external modules
        self.push(
            LabeledInstruction::builder(OpCode::PASS).payload(lambda_function.syntax_object_id),
        );

        // Save how many locals we have, for when we hit lets
        self.local_count.push(arity);

        let mut body_instructions = {
            let mut code_gen = CodeGenerator::new(self.constant_map, self.analysis);
            code_gen.visit(&lambda_function.body)?;
            code_gen.instructions
        };

        // In the event we actually have a closure, we need to add the necessarily
        // boilerplate to lift out closed over variables since they could escape
        if op_code == OpCode::NEWSCLOSURE {
            // Mark the upvalues here
            self.push(
                LabeledInstruction::builder(OpCode::NDEFS)
                    .payload(function_info.captured_vars().len()),
            );

            let mut vars = function_info.captured_vars().iter().collect::<Vec<_>>();

            vars.sort_by_key(|x| x.1.id);

            // vars.sort_by_key(|x| x.stack_offset);

            // Here we're going to explicitly capture from either the enclosing scope
            // or the stack. For example:
            //
            // (lambda (x)
            //      (lambda (y)
            //            (+ x y)))
            //
            // The inner lambda here will capture x from the stack, since that
            // is the environment in which it was immediately available.
            // Whereas:
            //
            // (lambda (x)
            //      (lambda (y)
            //            (lambda (z)
            //                   (+ x y z))))
            //
            // The innermost lambda is going to have to capture x and y from the closure above it,
            // where they've already been captured.
            //
            // This way, at closure construction (in the VM) we can immediately patch in the kind
            // of closure that we want to create, and where to get it
            for (key, var) in vars {
                // If we're patching in from the enclosing, check to see if this is a heap allocated var that
                // we need to patch in to the current scope
                if var.captured_from_enclosing {
                    if var.mutated {
                        self.push(
                            LabeledInstruction::builder(OpCode::COPYHEAPCAPTURECLOSURE)
                                .payload(var.parent_heap_offset.unwrap())
                                .contents(SyntaxObject::default(TokenType::Identifier(
                                    key.to_string(),
                                ))),
                        );
                    } else {
                        // In this case we're gonna patch in the variable from the current captured scope
                        self.push(
                            LabeledInstruction::builder(OpCode::COPYCAPTURECLOSURE)
                                .payload(var.capture_offset.unwrap())
                                .contents(SyntaxObject::default(TokenType::Identifier(
                                    key.to_string(),
                                ))),
                        );
                    }
                } else if var.mutated {
                    self.push(
                        LabeledInstruction::builder(OpCode::FIRSTCOPYHEAPCAPTURECLOSURE)
                            .payload(var.heap_offset.unwrap())
                            .contents(SyntaxObject::default(TokenType::Identifier(
                                key.to_string(),
                            ))),
                    );
                } else {
                    // In this case, it hasn't yet been captured, so we'll just capture
                    // directly from the stack
                    self.push(
                        LabeledInstruction::builder(OpCode::COPYCAPTURESTACK)
                            .payload(var.stack_offset.unwrap())
                            .contents(SyntaxObject::default(TokenType::Identifier(
                                key.to_string(),
                            ))),
                    );
                }
            }
        }

        let pop_op_code = OpCode::POPPURE;

        body_instructions
            .push(LabeledInstruction::builder(pop_op_code).payload(lambda_function.args.len()));

        // Load in the heap alloc instructions - on each invocation we can copy in to the current frame
        {
            let mut captured_mutable_arguments = function_info
                .arguments()
                .iter()
                // .values()
                .filter(|x| x.1.captured && x.1.mutated)
                .collect::<Vec<_>>();

            captured_mutable_arguments.sort_by_key(|x| x.1.stack_offset);

            for (key, var) in captured_mutable_arguments {
                self.push(
                    LabeledInstruction::builder(OpCode::ALLOC)
                        .payload(var.stack_offset.unwrap())
                        .contents(SyntaxObject::default(TokenType::Identifier(
                            key.to_string(),
                        ))),
                );
            }
        }

        self.instructions.append(&mut body_instructions);

        // pop off the local variables from the run time stack, so we don't have them

        let closure_body_size = self.len() - idx;
        self.push(LabeledInstruction::builder(OpCode::ECLOSURE).payload(arity));

        if let Some(elem) = self.instructions.get_mut(idx) {
            elem.payload_size = closure_body_size;
        } else {
            stop!(Generic => "out of bounds closure len");
        }

        self.local_count.pop();

        Ok(())
    }

    fn visit_begin(&mut self, begin: &crate::parser::ast::Begin) -> Self::Output {
        if begin.exprs.is_empty() {
            self.push(LabeledInstruction::builder(OpCode::VOID));
            return Ok(());
        }

        for expr in &begin.exprs {
            self.visit(expr)?;
        }

        if begin.exprs.len() > 1 {
            self.push(LabeledInstruction::builder(OpCode::POPN).payload(begin.exprs.len() - 1));
        }

        Ok(())
    }

    fn visit_return(&mut self, r: &crate::parser::ast::Return) -> Self::Output {
        self.visit(&r.expr)?;
        self.push(LabeledInstruction::builder(OpCode::POPPURE));
        Ok(())
    }

    fn visit_quote(&mut self, quote: &crate::parser::ast::Quote) -> Self::Output {
        let converted =
            SteelVal::try_from(crate::parser::ast::ExprKind::Quote(Box::new(quote.clone())))?;

        let idx = self.constant_map.add_or_get(converted);
        self.push(
            LabeledInstruction::builder(OpCode::PUSHCONST)
                .payload(idx)
                .constant(true),
        );

        Ok(())
    }

    fn visit_macro(&mut self, m: &crate::parser::ast::Macro) -> Self::Output {
        stop!(BadSyntax => "unexpected macro definition"; m.location.span)
    }

    fn visit_atom(&mut self, a: &crate::parser::ast::Atom) -> Self::Output {
        if let Some(analysis) = self.analysis.get(&a.syn) {
            let op_code = match (&analysis.kind, analysis.last_usage) {
                (Global, _) => OpCode::PUSH,
                (Local, true) | (LetVar, true) => OpCode::MOVEREADLOCAL,
                // (Local, true) | (LetVar, true) => OpCode::READLOCAL,
                (Local, false) | (LetVar, false) => OpCode::READLOCAL,

                (LocallyDefinedFunction, _) => {
                    stop!(Generic => "Unable to lower to bytecode: locally defined function should be lifted to an external scope")
                }
                // In the event we're captured, we're going to just read the
                // offset from the captured var
                (Captured, _) => OpCode::READCAPTURED,
                (Free, _) => OpCode::PUSH,
                (HeapAllocated, _) => OpCode::READALLOC,
                // This is technically true, but in an incremental compilation mode, we assume the variable is already bound
                // stop!(FreeIdentifier => format!("free identifier: {}", a); a.syn.span),
            };

            // println!("Atom: {}", a);
            // println!("{:#?}", analysis);

            let payload = match op_code {
                OpCode::READCAPTURED => analysis.read_capture_offset.unwrap(),
                OpCode::READALLOC => analysis.read_heap_offset.unwrap(),
                _ => analysis.stack_offset.unwrap_or_default(),
            };

            self.push(
                LabeledInstruction::builder(op_code)
                    .payload(payload)
                    .contents(a.syn.clone()),
            );

            Ok(())
        } else {
            self.specialize_constant(&a.syn)
        }
    }

    // TODO: Specialize the calls to binops here
    // This should be pretty straightforward - just check if they're still globals
    // then, specialize accordingly.
    fn visit_list(&mut self, l: &crate::parser::ast::List) -> Self::Output {
        // TODO: Come back to call specialization
        // if let Some(op) = self.should_specialize_call(l) {
        //     return self.specialize_call(l, op);
        // }

        if l.args.is_empty() {
            stop!(BadSyntax => "function application empty");
        }

        let pop_len = if !l.args.is_empty() {
            l.args[1..].len()
        } else {
            0
        };

        for expr in &l.args[1..] {
            self.visit(expr)?;
        }

        // emit instructions for the func
        self.visit(&l.args[0])?;

        let contents = if let ExprKind::Atom(Atom { syn: s }) = &l.args[0] {
            s.clone()
        } else {
            // TODO check span information here by coalescing the entire list
            SyntaxObject::new(
                TokenType::Identifier("lambda".to_string()),
                get_span(&l.args[0]),
            )
        };

        if let Some(call_info) = self.analysis.call_info.get(&l.syntax_object_id) {
            let op_code = match call_info.kind {
                Normal => OpCode::FUNC,
                TailCall => OpCode::TAILCALL,
                SelfTailCall(_) => {
                    // We don't need to push the function onto the stack if we're doing a self
                    // tail call
                    self.instructions.pop();
                    OpCode::TCOJMP
                    // OpCode::TAILCALL
                }
            };

            self.push(
                LabeledInstruction::builder(op_code)
                    .contents(contents)
                    .payload(pop_len),
            );

            if let SelfTailCall(depth) = call_info.kind {
                self.push(LabeledInstruction::builder(OpCode::PASS).payload(depth - 1));
            }

            Ok(())
        } else {
            self.push(
                LabeledInstruction::builder(OpCode::FUNC)
                    .contents(contents)
                    .payload(pop_len),
            );

            Ok(())

            // stop!(Generic => "Unable to find analysis information for call site!")
        }
    }

    fn visit_syntax_rules(&mut self, l: &crate::parser::ast::SyntaxRules) -> Self::Output {
        stop!(BadSyntax => "unexpected syntax rules"; l.location.span)
    }

    fn visit_set(&mut self, s: &crate::parser::ast::Set) -> Self::Output {
        // stop!(BadSyntax => "set! is currently not implemented"; s.location.span);

        self.visit(&s.expr)?;

        let a = s.variable.atom_syntax_object().unwrap();

        if let Some(analysis) = self.analysis.get(a) {
            // TODO: We really want to fail as early as possible, but lets just try this here:
            if analysis.builtin {
                stop!(Generic => "set!: cannot mutate module-required identifier"; s.location.span);
            }

            let op_code = match &analysis.kind {
                Global => OpCode::SET,
                Local | LetVar => OpCode::SETLOCAL,

                LocallyDefinedFunction => {
                    stop!(Generic => "Unable to lower to bytecode: locally defined function should be lifted to an external scope")
                }
                // In the event we're captured, we're going to just read the
                // offset from the captured var
                Captured => {
                    stop!(Generic => "Compiler error: Should not be able to set! an immutably captured variable")
                }
                Free => OpCode::SET,
                HeapAllocated => OpCode::SETALLOC,
                // This is technically true, but in an incremental compilation mode, we assume the variable is already bound
                // stop!(FreeIdentifier => format!("free identifier: {}", a); a.syn.span),
            };

            let payload = match op_code {
                // OpCode::SETCAPTURED => analysis.read_capture_offset.unwrap(),
                OpCode::SETALLOC => analysis.read_heap_offset.unwrap(),
                _ => analysis.stack_offset.unwrap_or_default(),
            };

            self.push(
                LabeledInstruction::builder(op_code)
                    .payload(payload)
                    .contents(a.clone()),
            );

            Ok(())
        } else {
            stop!(Generic => "Something went wrong with getting the var in set!"; s.location.span);
        }
    }

    fn visit_require(&mut self, r: &crate::parser::ast::Require) -> Self::Output {
        stop!(BadSyntax => "unexpected require statement in code gen"; r.location.span)
    }

    fn visit_let(&mut self, l: &crate::parser::ast::Let) -> Self::Output {
        // What we're gonna do here is pretty straight forward:
        // Since we're entering a scope, we need to include the code to remove from this from
        // the stack as well
        //
        // Otherwise, the machinery is more or less the same as before - we're just not going to
        // enter a new section of bytecode, its all going to live under the same block.
        //
        // (let ((a 10) (b 20))
        //      (+ 1 2 3 4 5) <--- Could get dropped immediately, but for now it does not
        //      (+ 2 3 4 5 6) <--|
        //      (+ a b))
        //
        // This should result in something like
        // PUSHCONST 10
        // PUSHCONST 20
        // READLOCAL a
        // READLOCAL b
        // PUSH +
        // FUNC 2
        // LETENDSCOPE 0 <- index of the stack when we entered this let expr

        self.push(
            LabeledInstruction::builder(OpCode::BEGINSCOPE)
                .payload(*self.local_count.last().unwrap_or(&0)),
        );

        let info = self
            .analysis
            .let_info
            .get(&l.syntax_object_id)
            .expect("Missing analysis information for let");

        // Push the scope + the number of arguments in this scope
        // self.push(LabeledInstruction::builder(OpCode::BEGINSCOPE).payload(l.bindings.len()));
        // self.push(LabeledInstruction::builder(OpCode::BEGINSCOPE).payload(info.stack_offset));

        // We just assume these will live on the stack at whatever position we're entering now
        for expr in l.expression_arguments() {
            self.visit(expr)?;
            // For the JIT -> push the last instruction to the internal scope
            // TODO: Rename from BEGINSCOPE to something like MARKLETVAR
            // self.push(LabeledInstruction::builder(OpCode::LetVar));
        }

        let mut heap_allocated_arguments = info
            .arguments
            .values()
            .filter(|x| x.captured && x.mutated)
            .collect::<Vec<_>>();

        heap_allocated_arguments.sort_by_key(|x| x.stack_offset);

        for var in heap_allocated_arguments {
            // println!("Found a var that is both mutated and captured");
            // println!("{:#?}", var);

            self.push(
                LabeledInstruction::builder(OpCode::ALLOC).payload(var.stack_offset.unwrap()),
            );
        }

        self.visit(&l.body_expr)?;

        // TODO:
        // It is possible, that during the course of execution, local variables get captured.
        // For example:
        //
        // (let ((a 10) (b 20))
        //      (lambda (x) (+ x a b)))
        //
        // In this case, at the end of the let scope, we can move those variables
        // into the slot that exists on the heap - and local variable references internally
        // in the function will refer to the local function slots, rather than the spots on the stack
        //
        // Having things on the stack will make things much faster. Lets try to keep them there.
        //
        // The let will have to keep track of if any of these values are captured, and if they are
        // just insert an instruction to close that upvalue

        // TODO: Add handling in the VM for this, as well as understanding how
        // upvalues are going to get closed when exiting the stack
        self.push(LabeledInstruction::builder(OpCode::LETENDSCOPE).payload(info.stack_offset));

        Ok(())
    }
}

#[cfg(test)]
mod code_gen_tests {
    use super::*;

    use crate::parser::parser::Parser;

    #[test]
    fn check_function_calls() {
        let expr = r#"
        ;; (define fib (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))

        (fib 10)
            "#;

        let exprs = Parser::parse(expr).unwrap();
        let mut analysis = Analysis::from_exprs(&exprs);
        analysis.populate_captures(&exprs);

        let mut constants = ConstantMap::new();

        let mut code_gen = CodeGenerator::new(&mut constants, &analysis);

        code_gen.visit(&exprs[0]).unwrap();
        // code_gen.visit(&exprs[1]).unwrap();

        println!("{:#?}", code_gen.instructions);
    }

    #[test]
    fn check_fib() {
        let expr = r#"
        (define fib (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))

        (fib 10)
            "#;

        let exprs = Parser::parse(expr).unwrap();
        let mut analysis = Analysis::from_exprs(&exprs);
        analysis.populate_captures(&exprs);

        let mut constants = ConstantMap::new();

        let mut code_gen = CodeGenerator::new(&mut constants, &analysis);

        code_gen.visit(&exprs[0]).unwrap();
        code_gen.visit(&exprs[1]).unwrap();

        println!("{:#?}", code_gen.instructions);
    }

    #[test]
    fn check_new_closure() {
        let expr = r#"
            (lambda (x)
                (lambda (y) (+ x y)))
        "#;

        let exprs = Parser::parse(expr).unwrap();
        let mut analysis = Analysis::from_exprs(&exprs);
        analysis.populate_captures(&exprs);

        let mut constants = ConstantMap::new();

        let mut code_gen = CodeGenerator::new(&mut constants, &analysis);

        code_gen.visit(&exprs[0]).unwrap();

        println!("{:#?}", code_gen.instructions);
    }

    #[test]
    fn check_lambda_output() {
        let expr = r#"
        (lambda (x y z)
            (+ x y z))
        "#;

        let exprs = Parser::parse(expr).unwrap();

        let mut analysis = Analysis::from_exprs(&exprs);
        analysis.populate_captures(&exprs);

        let mut constants = ConstantMap::new();

        let mut code_gen = CodeGenerator::new(&mut constants, &analysis);

        code_gen.visit(&exprs[0]).unwrap();

        println!("{:#?}", code_gen.instructions);

        let expected = vec![
            (OpCode::PUREFUNC, 9), // This captures no variables - should be able to be lifted as well
            (OpCode::PASS, 0),     // multi arity
            (OpCode::PASS, 0),     // This shouldn't need to be here
            (OpCode::MOVEREADLOCAL, 0), // last usage of x, first var
            (OpCode::MOVEREADLOCAL, 1), // last usage of y
            (OpCode::MOVEREADLOCAL, 2), // last usage of z
            (OpCode::PUSH, 0), // + is a global, that is late bound and the index is resolved later
            (OpCode::TAILCALL, 3), // tail call function, with 3 arguments
            (OpCode::POPPURE, 3), // Pop 3 arguments
            (OpCode::ECLOSURE, 3), // Something about 3 arguments...
        ];

        let mut found = code_gen
            .instructions
            .iter()
            .map(|x| (x.op_code, x.payload_size))
            .collect::<Vec<_>>();

        // Wipe out the syntax object id from the PASS
        found[2].1 = 0;

        assert_eq!(expected, found);
    }

    #[test]
    fn check_let_captured_var() {
        let expr = r#"
        (%plain-let ((a 10) (b 20))
            (lambda () (+ a b)))
        "#;

        let exprs = Parser::parse(expr).unwrap();

        let mut analysis = Analysis::from_exprs(&exprs);
        analysis.populate_captures(&exprs);

        let mut constants = ConstantMap::new();

        let mut code_gen = CodeGenerator::new(&mut constants, &analysis);

        code_gen.visit(&exprs[0]).unwrap();
    }

    #[test]
    fn check_let_output() {
        let expr = r#"
            (%plain-let ((a 10) (b 20))
                (+ a b))
        "#;

        let exprs = Parser::parse(expr).unwrap();

        let analysis = Analysis::from_exprs(&exprs);
        let mut constants = ConstantMap::new();

        let mut code_gen = CodeGenerator::new(&mut constants, &analysis);

        code_gen.visit(&exprs[0]).unwrap();

        println!("{:#?}", code_gen.instructions);

        let expected = vec![
            (OpCode::BEGINSCOPE, 0),
            (OpCode::PUSHCONST, 0),   // Should be the only constant in the map
            (OpCode::PUSHCONST, 1),   // Should be the second constant in the map
            (OpCode::READLOCAL, 0),   // Corresponds to index 0
            (OpCode::READLOCAL, 1),   // Corresponds to index 1
            (OpCode::PUSH, 0), // + is a global, that is late bound and the index is resolved later
            (OpCode::FUNC, 2), // Function call with 2 arguments
            (OpCode::LETENDSCOPE, 2), // Exit the let scope and drop the vars and anything above it
        ];

        let found = code_gen
            .instructions
            .iter()
            .map(|x| (x.op_code, x.payload_size))
            .collect::<Vec<_>>();

        assert_eq!(expected, found);
    }
}
