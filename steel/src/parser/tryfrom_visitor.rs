use crate::parser::ast::ExprKind;
use crate::parser::visitors::ConsumingVisitorRef;

use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};

use super::ast::Atom;

use std::convert::TryFrom;

use crate::gc::Gc;
use crate::primitives::ListOperations;

pub struct TryFromExprKindForSteelVal {}

impl TryFromExprKindForSteelVal {
    pub fn try_from_expr_kind(e: ExprKind) -> Result<Gc<SteelVal>> {
        TryFromExprKindForSteelVal {}.visit(e)
    }
}

impl ConsumingVisitorRef for TryFromExprKindForSteelVal {
    type Output = Result<Gc<SteelVal>>;

    fn visit_if(&self, f: Box<super::ast::If>) -> Self::Output {
        let expr = [
            Gc::new(SteelVal::try_from(f.location)?),
            self.visit(f.test_expr)?,
            self.visit(f.then_expr)?,
            self.visit(f.else_expr)?,
        ];
        ListOperations::built_in_list_func_flat(&expr)
    }

    fn visit_define(&self, define: Box<super::ast::Define>) -> Self::Output {
        todo!()
    }

    fn visit_lambda_function(
        &self,
        lambda_function: Box<super::ast::LambdaFunction>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_begin(&self, begin: super::ast::Begin) -> Self::Output {
        todo!()
    }

    fn visit_return(&self, r: Box<super::ast::Return>) -> Self::Output {
        todo!()
    }

    fn visit_apply(&self, apply: Box<super::ast::Apply>) -> Self::Output {
        todo!()
    }

    fn visit_panic(&self, p: Box<super::ast::Panic>) -> Self::Output {
        todo!()
    }

    fn visit_transduce(&self, transduce: Box<super::ast::Transduce>) -> Self::Output {
        let expr = [
            Gc::new(SteelVal::try_from(transduce.location)?),
            self.visit(transduce.transducer)?,
            self.visit(transduce.func)?,
            self.visit(transduce.initial_value)?,
            self.visit(transduce.iterable)?,
        ];
        ListOperations::built_in_list_func_flat(&expr)
    }

    fn visit_read(&self, read: Box<super::ast::Read>) -> Self::Output {
        let expr = [
            Gc::new(SteelVal::try_from(read.location)?),
            self.visit(read.expr)?,
        ];
        ListOperations::built_in_list_func_flat(&expr)
    }

    fn visit_execute(&self, execute: Box<super::ast::Execute>) -> Self::Output {
        let mut exprs = vec![
            Gc::new(SteelVal::try_from(execute.location)?),
            self.visit(execute.transducer)?,
            self.visit(execute.collection)?,
        ];

        if let Some(output) = execute.output_type {
            exprs.push(self.visit(output)?);
        }
        ListOperations::built_in_list_func_flat(&exprs)
    }

    fn visit_quote(&self, quote: Box<super::ast::Quote>) -> Self::Output {
        self.visit(quote.expr)
    }

    fn visit_struct(&self, s: Box<super::ast::Struct>) -> Self::Output {
        todo!()
    }

    fn visit_macro(&self, m: super::ast::Macro) -> Self::Output {
        todo!()
    }

    fn visit_eval(&self, e: Box<super::ast::Eval>) -> Self::Output {
        todo!()
    }

    fn visit_atom(&self, a: Atom) -> Self::Output {
        SteelVal::try_from(a.syn).map(Gc::new)
    }

    fn visit_list(&self, l: super::ast::List) -> Self::Output {
        let items: std::result::Result<Vec<Gc<SteelVal>>, SteelErr> =
            l.args.into_iter().map(|x| self.visit(x)).collect();

        ListOperations::built_in_list_func_flat(&items?)
    }

    fn visit_syntax_rules(&self, l: super::ast::SyntaxRules) -> Self::Output {
        todo!()
    }

    fn visit_set(&self, s: Box<super::ast::Set>) -> Self::Output {
        todo!()
    }
}
