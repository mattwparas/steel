use crate::{
    compiler::passes::analysis::IdentifierStatus::{
        Captured, Free, Global, LetVar, Local, LocallyDefinedFunction,
    },
    core::{
        labels::{LabelGenerator, LabeledInstruction},
        opcode::OpCode,
    },
    parser::{
        ast::{Atom, ExprKind},
        parser::SyntaxObject,
        span_visitor::get_span,
        tokens::TokenType,
        visitors::VisitorMut,
    },
    stop, SteelVal,
};

use super::{
    constants::{ConstantMap, ConstantTable},
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
    label_maker: LabelGenerator,
}

fn eval_atom(t: &SyntaxObject) -> Result<SteelVal> {
    match &t.ty {
        TokenType::BooleanLiteral(b) => Ok((*b).into()),
        // TokenType::Identifier(s) => env.borrow().lookup(&s),
        TokenType::NumberLiteral(n) => Ok(SteelVal::NumV(*n)),
        TokenType::StringLiteral(s) => Ok(SteelVal::StringV(s.clone().into())),
        TokenType::CharacterLiteral(c) => Ok(SteelVal::CharV(*c)),
        TokenType::IntegerLiteral(n) => Ok(SteelVal::IntV(*n)),
        // TODO: Keywords shouldn't be misused as an expression - only in function calls are keywords allowed
        TokenType::Keyword(k) => Ok(SteelVal::SymbolV(k.clone().into())),
        what => {
            println!("getting here in the eval_atom");
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
            label_maker: LabelGenerator::default(),
        }
    }

    fn push(&mut self, instr: LabeledInstruction) {
        self.instructions.push(instr);
    }

    fn len(&self) -> usize {
        self.instructions.len()
    }

    fn specialize_constant(&mut self, syn: &SyntaxObject) -> Result<()> {
        let value = eval_atom(syn)?;

        let opcode = match &value {
            // SteelVal::IntV(1) => OpCode::LOADINT1,
            // SteelVal::IntV(2) => OpCode::LOADINT2,
            _ => OpCode::PUSHCONST,
        };

        let idx = self.constant_map.add_or_get(value);
        self.push(
            LabeledInstruction::builder(opcode)
                .payload(idx)
                .contents(syn.clone())
                .constant(true),
        );
        Ok(())
    }
}

impl<'a> VisitorMut for CodeGenerator<'a> {
    type Output = Result<()>;

    fn visit_if(&mut self, f: &crate::parser::ast::If) -> Self::Output {
        self.visit(&f.test_expr)?;

        let if_idx = self.instructions.len();

        self.push(LabeledInstruction::builder(OpCode::IF).payload(self.instructions.len() + 2));

        self.visit(&f.then_expr)?;

        let false_start_label = self.label_maker.fresh();

        self.push(LabeledInstruction::builder(OpCode::JMP).goto(false_start_label));
        let false_start = self.len();

        self.visit(&f.else_expr)?;

        let j3_label = self.label_maker.fresh();

        self.instructions.last_mut().unwrap().set_tag(j3_label);

        if let Some(elem) = self.instructions.get_mut(false_start - 1) {
            // (*elem).goto = Some(j3);

            elem.set_goto(j3_label);

            // (*elem).payload_size = false_start;
        } else {
            stop!(Generic => "out of bounds jump");
        }

        if let Some(elem) = self.instructions.get_mut(if_idx) {
            // (*elem).goto = Some(false_start_label);

            elem.set_goto(false_start_label);

            // (*elem).payload_size = false_start;
        } else {
            stop!(Generic => "out of bounds jump");
        }

        Ok(())
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
                (define.name).to_string()
            )
        }

        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &crate::parser::ast::LambdaFunction,
    ) -> Self::Output {
        let idx = self.len();

        self.push(LabeledInstruction::builder(OpCode::SCLOSURE));

        self.push(
            LabeledInstruction::builder(OpCode::PASS).payload(if lambda_function.rest {
                1
            } else {
                0
            }),
        );

        // Patching over the changes, see old code generator for more information
        self.push(LabeledInstruction::builder(OpCode::PASS));

        let mut body_instructions = {
            let mut code_gen = CodeGenerator::new(&mut self.constant_map, &self.analysis);
            code_gen.visit(&lambda_function.body)?;
            code_gen.instructions
        };

        let function_info = self
            .analysis
            .function_info
            .get(&lambda_function.syntax_object_id)
            .unwrap();

        // Mark the upvalues here
        self.push(
            LabeledInstruction::builder(OpCode::NDEFS).payload(function_info.captured_vars().len()),
        );

        // TODO:
        // Go through each of the vars an explicitly capture them
        // This does not match explicitly what the upvalues are doing, and in fact
        // We don't plan on imitating that behavior moving forward
        //
        // Moving forward, there will simply be a new constructor for lambdas, which
        // explicitly captures the variables made.
        //
        // Something like (make-closure (vars) (captured-vars) exprs...)
        //
        // This way, at run time we can simply allocate these values directly onto the heap
        // and subsequently store the weak refs in the closure object, which there will be separate
        // objects for, for pure functions and closures
        for var in function_info.captured_vars() {
            self.push(LabeledInstruction::builder(OpCode::FILLLOCALUPVALUE))
        }

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

        Ok(())
    }

    fn visit_return(&mut self, r: &crate::parser::ast::Return) -> Self::Output {
        self.visit(&r.expr)?;
        self.push(LabeledInstruction::builder(OpCode::POP));
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

    fn visit_struct(&mut self, s: &crate::parser::ast::Struct) -> Self::Output {
        todo!()
    }

    fn visit_macro(&mut self, m: &crate::parser::ast::Macro) -> Self::Output {
        stop!(BadSyntax => "unexpected macro definition"; m.location.span)
    }

    fn visit_atom(&mut self, a: &crate::parser::ast::Atom) -> Self::Output {
        if let Some(analysis) = self.analysis.get(&a.syn) {
            let op_code = match (&analysis.kind, analysis.last_usage) {
                (Global, _) => OpCode::PUSH,
                (Local, true) | (LetVar, true) => OpCode::MOVEREADLOCAL,
                (Local, false) | (LetVar, false) => OpCode::READLOCAL,

                (LocallyDefinedFunction, _) => {
                    stop!(Generic => "Unable to lower to bytecode: locally defined function should be lifted to an external scope")
                }
                (Captured, true) => OpCode::MOVEREADUPVALUE,
                (Captured, false) => OpCode::READUPVALUE,
                (Free, _) => stop!(FreeIdentifier => format!("free identifier: {}", a); a.syn.span),
            };

            self.push(
                LabeledInstruction::builder(op_code)
                    .payload(analysis.stack_offset.unwrap_or_default())
                    .contents(a.syn.clone()),
            );

            Ok(())
        } else {
            return self.specialize_constant(&a.syn);
        }
    }

    fn visit_list(&mut self, l: &crate::parser::ast::List) -> Self::Output {
        if l.args.is_empty() {
            stop!(BadSyntax => "function application empty");
        }

        let pop_len = if l.args.len() > 0 {
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
                TailCall => OpCode::TCOJMP,
                SelfTailCall => OpCode::TAILCALL,
            };

            self.push(
                LabeledInstruction::builder(op_code)
                    .contents(contents)
                    .payload(pop_len),
            );

            Ok(())
        } else {
            stop!(Generic => "Unable to find analysis information for call site!")
        }
    }

    fn visit_syntax_rules(&mut self, l: &crate::parser::ast::SyntaxRules) -> Self::Output {
        stop!(BadSyntax => "unexpected syntax rules"; l.location.span)
    }

    fn visit_set(&mut self, s: &crate::parser::ast::Set) -> Self::Output {
        todo!()
    }

    fn visit_require(&mut self, r: &crate::parser::ast::Require) -> Self::Output {
        stop!(BadSyntax => "unexpected require statement in code gen"; r.location.span)
    }

    fn visit_callcc(&mut self, cc: &crate::parser::ast::CallCC) -> Self::Output {
        self.visit(&cc.expr)?;
        self.push(LabeledInstruction::builder(OpCode::CALLCC));
        Ok(())
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
        //      (+ 1 2 3 4 5) <-| Could get dropped immediately, but for now it does not
        //      (+ 2 3 4 5 6) <-|
        //      (+ a b))
        //
        // This should result in something like
        // PUSHCONST 10
        // PUSHCONST 20
        // READLOCAL a
        // READLOCAL b
        // PUSH +
        // FUNC 2
        // ENDSCOPE 0 <- index of the stack when we entered this let expr

        // We just assume these will live on the stack at whatever position we're entering now
        for expr in l.expression_arguments() {
            self.visit(expr)?;
        }

        let info = self
            .analysis
            .let_info
            .get(&l.syntax_object_id)
            .expect("Missing analysis information for let");

        self.visit(&l.body_expr)?;

        // TODO: Add handling in the VM for this, as well as understanding how
        // upvalues are going to get closed when exiting the stack
        self.push(LabeledInstruction::builder(OpCode::LETENDSCOPE).payload(info.stack_offset));

        Ok(())
    }
}

#[cfg(test)]
mod code_gen_tests {
    use super::*;

    use crate::{parser::parser::Parser, rerrs::ErrorKind};

    #[test]
    fn check_let_output() {
        let expr = r#"

            (define + "dummy")

            (%plain-let ((a 10) (b 20))
                (+ a b))
        "#;

        let exprs = Parser::parse(expr).unwrap();

        let analysis = Analysis::from_exprs(&exprs);
        let mut constants = ConstantMap::new();

        let mut code_gen = CodeGenerator::new(&mut constants, &analysis);

        code_gen.visit(&exprs[1]).unwrap();

        println!("{:#?}", code_gen.instructions);

        let expected = vec![
            (OpCode::PUSHCONST, 0),   // Should be the only constant in the map
            (OpCode::PUSHCONST, 1),   // Should be the second constant in the map
            (OpCode::READLOCAL, 0),   // Corresponds to index 0
            (OpCode::READLOCAL, 1),   // Corresponds to index 1
            (OpCode::PUSH, 0), // + is a global, that is late bound and the index is resolved later
            (OpCode::FUNC, 2), // Function call with 2 arguments
            (OpCode::LETENDSCOPE, 0), // Exit the let scope and drop the vars and anything above it
        ];

        let found = code_gen
            .instructions
            .iter()
            .map(|x| (x.op_code, x.payload_size))
            .collect::<Vec<_>>();

        assert_eq!(expected, found);
    }
}
