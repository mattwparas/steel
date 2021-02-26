use std::convert::TryFrom;

use super::constants::{ConstantMap, ConstantTable};
use crate::{
    core::{instructions::Instruction, opcode::OpCode},
    parser::{ast::Atom, parser::SyntaxObject, span_visitor::get_span, tokens::TokenType},
};

use crate::parser::ast::ExprKind;
use crate::parser::visitors::VisitorMut;

use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;

use log::info;

// use super::codegen::{check_and_transform_mutual_recursion, transform_tail_call};

pub struct CodeGenerator<'a> {
    instructions: Vec<Instruction>,
    constant_map: &'a mut ConstantMap,
    defining_context: Option<String>,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(constant_map: &'a mut ConstantMap) -> Self {
        CodeGenerator {
            instructions: Vec::new(),
            constant_map,
            defining_context: None,
        }
    }

    pub fn new_from_body_instructions(
        constant_map: &'a mut ConstantMap,
        instructions: Vec<Instruction>,
    ) -> Self {
        CodeGenerator {
            instructions,
            constant_map,
            defining_context: None,
        }
    }

    pub fn compile(mut self, expr: &ExprKind) -> Result<Vec<Instruction>> {
        self.visit(expr)?;
        Ok(self.instructions)
    }

    #[inline]
    fn push(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    #[inline]
    fn len(&self) -> usize {
        self.instructions.len()
    }
}

impl<'a> VisitorMut for CodeGenerator<'a> {
    type Output = Result<()>;

    fn visit_if(&mut self, f: &crate::parser::ast::If) -> Self::Output {
        // load in the test condition
        self.visit(&f.test_expr)?;
        // push in if
        self.push(Instruction::new_if(self.instructions.len() + 2));
        // save spot of jump instruction, fill in after
        let idx = self.len();
        self.push(Instruction::new_jmp(0)); // dummy value

        // emit instructions for then
        self.visit(&f.then_expr)?;
        self.push(Instruction::new_jmp(0));
        let false_start = self.len();

        // emit instructions for else expression
        self.visit(&f.else_expr)?;
        let j3 = self.len(); // first instruction after else

        // set index of jump instruction
        if let Some(elem) = self.instructions.get_mut(idx) {
            (*elem).payload_size = false_start;
        } else {
            stop!(Generic => "out of bounds jump");
        }

        if let Some(elem) = self.instructions.get_mut(false_start - 1) {
            (*elem).payload_size = j3;
        } else {
            stop!(Generic => "out of bounds jump");
        }

        Ok(())
    }

    fn visit_define(&mut self, define: &crate::parser::ast::Define) -> Self::Output {
        // todo!()

        let sidx = self.len();
        self.push(Instruction::new_sdef());

        if let ExprKind::Atom(name) = &define.name {
            let defining_context = if let TokenType::Identifier(ident) = &name.syn.ty {
                if let Some(x) = self.instructions.get_mut(sidx) {
                    x.contents = Some(name.syn.clone());
                }
                Some(ident.clone())
            } else {
                None
            };

            // Set this for tail call optimization ease
            self.defining_context = defining_context;

            self.visit(&define.body)?;
            self.push(Instruction::new_pop());
            let defn_body_size = self.len() - sidx;
            self.push(Instruction::new_edef());

            if let Some(elem) = self.instructions.get_mut(sidx) {
                (*elem).payload_size = defn_body_size;
            } else {
                stop!(Generic => "out of bounds closure len");
            }

            self.push(Instruction::new_bind(name.syn.clone()));
            self.push(Instruction::new_void());

            // Clean up the defining context state
            self.defining_context = None;
        } else {
            panic!("Complex defines not supported in bytecode generation")
        }

        Ok(())
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &crate::parser::ast::LambdaFunction,
    ) -> Self::Output {
        // todo!()

        let idx = self.len();
        self.push(Instruction::new_sclosure());
        self.push(Instruction::new_ndef(0)); // Default with 0 for now

        let mut body_instructions = Vec::new();
        let arity;

        let l = &lambda_function.args;

        arity = l.len();
        let rev_iter = l.iter().rev();
        for symbol in rev_iter {
            if let ExprKind::Atom(atom) = symbol {
                match &atom.syn {
                    SyntaxObject {
                        ty: TokenType::Identifier(_),
                        ..
                    } => body_instructions.push(Instruction::new_bind(atom.syn.clone())),
                    SyntaxObject {
                        ty: _, span: sp, ..
                    } => {
                        stop!(Generic => "lambda function requires list of identifiers"; *sp);
                    }
                }
            } else {
                // stop!(Generic => "lambda function requires list of identifiers"; symbol.span());
                // TODO come back add the span
                stop!(Generic => "lambda function requires list of identifiers");
            }
        }

        // make recursive call with "fresh" vector so that offsets are correct
        body_instructions =
            CodeGenerator::new_from_body_instructions(&mut self.constant_map, body_instructions)
                .compile(&lambda_function.body)?;

        body_instructions.push(Instruction::new_pop());
        if let Some(ctx) = &self.defining_context {
            transform_tail_call(&mut body_instructions, ctx);
            let b = check_and_transform_mutual_recursion(&mut body_instructions);
            if b {
                info!("Transformed mutual recursion for: {}", ctx);
            }
        }

        self.instructions.append(&mut body_instructions);
        let closure_body_size = self.len() - idx;
        self.push(Instruction::new_eclosure(arity));

        if let Some(elem) = self.instructions.get_mut(idx) {
            (*elem).payload_size = closure_body_size;
        } else {
            stop!(Generic => "out of bounds closure len");
        }

        Ok(())
    }

    fn visit_begin(&mut self, begin: &crate::parser::ast::Begin) -> Self::Output {
        if begin.exprs.is_empty() {
            self.push(Instruction::new_void());
            return Ok(());
        }

        for expr in &begin.exprs {
            self.visit(expr)?;
        }
        Ok(())
    }

    fn visit_return(&mut self, r: &crate::parser::ast::Return) -> Self::Output {
        self.visit(&r.expr)?;
        // pop is equivalent to the last instruction in the function
        self.push(Instruction::new_pop());
        Ok(())
    }

    fn visit_apply(&mut self, apply: &crate::parser::ast::Apply) -> Self::Output {
        // todo!()
        self.visit(&apply.func)?;
        self.visit(&apply.list)?;
        self.push(Instruction::new_apply(apply.location.clone()));
        Ok(())
    }

    fn visit_panic(&mut self, p: &crate::parser::ast::Panic) -> Self::Output {
        // todo!()
        self.visit(&p.message)?;
        self.push(Instruction::new_panic(p.location.clone()));
        Ok(())
    }

    fn visit_transduce(&mut self, transduce: &crate::parser::ast::Transduce) -> Self::Output {
        self.visit(&transduce.transducer)?;
        self.visit(&transduce.func)?;
        self.visit(&transduce.initial_value)?;
        self.visit(&transduce.iterable)?;
        self.push(Instruction::new_transduce());
        Ok(())
    }

    fn visit_read(&mut self, read: &crate::parser::ast::Read) -> Self::Output {
        self.visit(&read.expr)?;
        self.push(Instruction::new_read());
        Ok(())
    }

    fn visit_execute(&mut self, execute: &crate::parser::ast::Execute) -> Self::Output {
        self.visit(&execute.transducer)?;
        self.visit(&execute.collection)?;

        if let Some(output_type) = &execute.output_type {
            self.visit(output_type)?;
            self.push(Instruction::new_collect_to());
        } else {
            self.push(Instruction::new_collect());
        }
        Ok(())
    }

    fn visit_quote(&mut self, quote: &crate::parser::ast::Quote) -> Self::Output {
        let converted = SteelVal::try_from(quote.expr.clone())?;
        let idx = self.constant_map.add_or_get(converted);
        self.push(Instruction::new_push_const(idx));

        Ok(())
    }

    fn visit_struct(&mut self, s: &crate::parser::ast::Struct) -> Self::Output {
        stop!(BadSyntax => "struct definition only allowed at top level"; s.location.span)
    }

    fn visit_macro(&mut self, m: &crate::parser::ast::Macro) -> Self::Output {
        stop!(BadSyntax => "unexpected macro definition"; m.location.span)
    }

    fn visit_eval(&mut self, e: &crate::parser::ast::Eval) -> Self::Output {
        self.visit(&e.expr)?;
        self.push(Instruction::new_eval());
        Ok(())
    }

    fn visit_atom(&mut self, a: &crate::parser::ast::Atom) -> Self::Output {
        self.push(Instruction::new(OpCode::PUSH, 0, a.syn.clone(), true));
        Ok(())
    }

    fn visit_list(&mut self, l: &crate::parser::ast::List) -> Self::Output {
        // dbg!(l);

        let pop_len = l.args[1..].len();

        // emit instructions for the args
        for expr in &l.args[1..] {
            self.visit(expr)?;
        }

        // emit instructions for the func
        self.visit(&l.args[0])?;

        if let ExprKind::Atom(Atom { syn: s }) = &l.args[0] {
            self.push(Instruction::new_func(pop_len, s.clone()));
        } else {
            // TODO check span information here by coalescing the entire list
            self.push(Instruction::new_func(
                pop_len,
                SyntaxObject::new(
                    TokenType::Identifier("lambda".to_string()),
                    get_span(&l.args[0]),
                ),
            ));
        }

        Ok(())
    }

    fn visit_syntax_rules(&mut self, l: &crate::parser::ast::SyntaxRules) -> Self::Output {
        stop!(BadSyntax => "unexpected syntax rules"; l.location.span)
    }

    fn visit_set(&mut self, s: &crate::parser::ast::Set) -> Self::Output {
        self.visit(&s.expr)?;
        if let ExprKind::Atom(Atom { syn: s }) = &s.variable {
            self.push(Instruction::new(OpCode::SET, 0, s.clone(), false));
        } else {
            stop!(BadSyntax => "set! takes an identifier")
        }
        Ok(())
    }
}

fn transform_tail_call(instructions: &mut Vec<Instruction>, defining_context: &str) -> bool {
    let last_idx = instructions.len() - 1;

    let mut indices = vec![last_idx];

    let mut transformed = false;

    for (idx, instruction) in instructions.iter().enumerate() {
        if instruction.op_code == OpCode::JMP && instruction.payload_size == last_idx {
            indices.push(idx);
        }
    }

    for index in &indices {
        if *index < 2 {
            continue;
        }
        let prev_instruction = instructions.get(index - 1);
        let prev_func_push = instructions.get(index - 2);

        match (prev_instruction, prev_func_push) {
            (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        }),
                    ..
                }),
            ) => {
                if s == defining_context {
                    let new_jmp = Instruction::new_jmp(0);
                    // inject tail call jump
                    instructions[index - 2] = new_jmp;
                    instructions[index - 1] = Instruction::new_pass();
                    transformed = true;

                    info!("Tail call optimization performed for: {}", defining_context);
                }
            }
            _ => {}
        }
    }

    transformed
}

// Note, this should be called AFTER `transform_tail_call`
fn check_and_transform_mutual_recursion(instructions: &mut [Instruction]) -> bool {
    let last_idx = instructions.len() - 1;

    // could panic
    let mut indices = vec![last_idx];

    let mut transformed = false;

    for (idx, instruction) in instructions.iter().enumerate() {
        if instruction.op_code == OpCode::JMP && instruction.payload_size == last_idx {
            indices.push(idx);
        }
    }

    for index in &indices {
        if *index < 2 {
            continue;
        }
        let prev_instruction = instructions.get(index - 1);
        let prev_func_push = instructions.get(index - 2);

        match (prev_instruction, prev_func_push) {
            (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(_s),
                            ..
                        }),
                    ..
                }),
            ) => {
                if let Some(x) = instructions.get_mut(index - 1) {
                    x.op_code = OpCode::TAILCALL;
                    transformed = true;
                }
            }
            _ => {}
        }
    }

    transformed
}
