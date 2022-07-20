use crate::{
    core::{
        labels::{LabelGenerator, LabeledInstruction},
        opcode::OpCode,
    },
    parser::{ast::ExprKind, tokens::TokenType, visitors::VisitorMut},
    stop,
};

use super::{constants::ConstantMap, passes::analysis::Analysis};

use crate::rvals::Result;

pub struct CodeGenerator<'a> {
    instructions: Vec<LabeledInstruction>,
    constant_map: &'a mut ConstantMap,
    analysis: &'a Analysis,
    label_maker: LabelGenerator,
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
        let sidx = self.len();

        if let ExprKind::Atom(name) = &define.name {
            self.push(LabeledInstruction::builder(OpCode::SDEF).contents(name.syn.clone()));

            self.visit(&define.body)?;

            let defn_body_size = self.len() - sidx;
            self.push(LabeledInstruction::builder(OpCode::EDEF));

            if let Some(elem) = self.instructions.get_mut(sidx) {
                (*elem).payload_size = defn_body_size;
            } else {
                stop!(Generic => "out of bounds closure len");
            }

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
        todo!()
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
        todo!()
    }

    fn visit_struct(&mut self, s: &crate::parser::ast::Struct) -> Self::Output {
        todo!()
    }

    fn visit_macro(&mut self, m: &crate::parser::ast::Macro) -> Self::Output {
        todo!()
    }

    fn visit_atom(&mut self, a: &crate::parser::ast::Atom) -> Self::Output {
        todo!()
    }

    fn visit_list(&mut self, l: &crate::parser::ast::List) -> Self::Output {
        todo!()
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
        todo!()
    }
}
