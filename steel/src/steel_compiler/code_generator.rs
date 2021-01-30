use super::constants::ConstantMap;
use crate::core::instructions::Instruction;

use crate::new_parser::ast::ExprKind;
use crate::new_parser::visitors::VisitorMut;

use crate::rerrs::SteelErr;
use crate::rvals::Result;
use crate::stop;

use log::info;

use super::codegen::{check_and_transform_mutual_recursion, transform_tail_call};

pub struct CodeGenerator<'a> {
    instructions: Vec<Instruction>,
    constant_map: &'a mut ConstantMap,
    defining_context: Option<String>,
}

impl<'a> CodeGenerator<'a> {
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

    fn visit_if(&mut self, f: &crate::new_parser::ast::If) -> Self::Output {
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

    fn visit_define(&mut self, define: &crate::new_parser::ast::Define) -> Self::Output {
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
        lambda_function: &crate::new_parser::ast::LambdaFunction,
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
                    SyntaxObject { ty: _, span: sp } => {
                        stop!(Generic => "lambda function requires list of identifiers"; *sp);
                    }
                }
            } else {
                // stop!(Generic => "lambda function requires list of identifiers"; symbol.span());
                // TODO come back add the span
                stop!(Generic => "lambda function requires list of identifiers");
            }
        }

        self.visit(&lambda_function.body)?;

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

    fn visit_begin(&mut self, begin: &crate::new_parser::ast::Begin) -> Self::Output {
        for expr in &begin.exprs {
            self.visit(expr)?;
        }
        Ok(())
    }

    fn visit_return(&mut self, r: &crate::new_parser::ast::Return) -> Self::Output {
        self.visit(&r.expr)?;
        // pop is equivalent to the last instruction in the function
        self.push(Instruction::new_pop());
        Ok(())
    }

    fn visit_apply(&mut self, apply: &crate::new_parser::ast::Apply) -> Self::Output {
        todo!()
    }

    fn visit_panic(&mut self, p: &crate::new_parser::ast::Panic) -> Self::Output {
        todo!()
    }

    fn visit_transduce(&mut self, transduce: &crate::new_parser::ast::Transduce) -> Self::Output {
        self.visit(&transduce.transducer)?;
        self.visit(&transduce.func)?;
        self.visit(&transduce.initial_value)?;
        self.visit(&transduce.iterable)?;
        self.push(Instruction::new_transduce());
        Ok(())
    }

    fn visit_read(&mut self, read: &crate::new_parser::ast::Read) -> Self::Output {
        self.visit(&read.expr)?;
        self.push(Instruction::new_read());
        Ok(())
    }

    fn visit_execute(&mut self, execute: &crate::new_parser::ast::Execute) -> Self::Output {
        self.visit(&execute.transducer)?;
        self.visit(&execute.collection)?;

        if let Some(output_type) = &execute.output_type {
            self.visit(output_type)?;
        }

        self.push(Instruction::new_collect());
        Ok(())
    }

    fn visit_quote(&mut self, quote: &crate::new_parser::ast::Quote) -> Self::Output {
        todo!()
    }

    fn visit_struct(&mut self, s: &crate::new_parser::ast::Struct) -> Self::Output {
        todo!()
    }

    fn visit_macro(&mut self, m: &crate::new_parser::ast::Macro) -> Self::Output {
        todo!()
    }

    fn visit_eval(&mut self, e: &crate::new_parser::ast::Eval) -> Self::Output {
        todo!()
    }

    fn visit_atom(&mut self, a: &crate::new_parser::ast::Atom) -> Self::Output {
        self.push(Instruction::new(OpCode::PUSH, 0, a.syn.clone(), true));
        Ok(())
    }

    fn visit_list(&mut self, l: &crate::new_parser::ast::List) -> Self::Output {
        todo!()
    }

    fn visit_define_function(&mut self, define: &crate::new_parser::ast::DefineFunction) -> Self::Output {
        todo!()
    }

    // fn visit_define_function()
}
