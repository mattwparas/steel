use crate::parser::ast::ExprKind;
use crate::parser::parser::SyntaxObject;
use crate::parser::tokens::TokenType;
use crate::parser::visitors::ConsumingVisitorRef;
// use crate::parser::span::Span;

use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};

use super::ast::Atom;

use std::{collections::HashMap, convert::TryFrom};

use crate::gc::Gc;
use crate::parser::expander::SteelMacro;
use crate::primitives::ListOperations;

//             Expr::VectorVal(lst) => {
//                 let items: std::result::Result<Vec<Gc<Self>>, Self::Error> = lst
//                     .iter()
//                     .map(|x| Self::try_from(x.clone()).map(Gc::new))
//                     .collect();

//                 ListOperations::built_in_list_func()(&items?).map(|x| (*x).clone())
//                 // Ok(VectorV(items?))

//                 // let items: std::result::Result<Vector<Self>, Self::Error> =
//                 //     lst.iter().map(|x| Self::try_from(x.clone())).collect();
//                 // Ok(VectorV(items?))
//             }
//         }
//     }
// }

pub struct TryFromExprKindForSteelVal {}

impl TryFromExprKindForSteelVal {
    pub fn try_from_expr_kind(e: ExprKind) -> Result<SteelVal> {
        TryFromExprKindForSteelVal {}.visit(e)
    }
}

impl ConsumingVisitorRef for TryFromExprKindForSteelVal {
    type Output = Result<SteelVal>;

    fn visit_if(&self, f: Box<super::ast::If>) -> Self::Output {
        todo!()
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
        todo!()
    }

    fn visit_read(&self, read: Box<super::ast::Read>) -> Self::Output {
        todo!()
    }

    fn visit_execute(&self, execute: Box<super::ast::Execute>) -> Self::Output {
        todo!()
    }

    fn visit_quote(&self, quote: Box<super::ast::Quote>) -> Self::Output {
        // todo!()

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
        SteelVal::try_from(a.syn)
    }

    fn visit_list(&self, l: super::ast::List) -> Self::Output {
        let items: std::result::Result<Vec<Gc<SteelVal>>, SteelErr> = l
            .args
            .into_iter()
            .map(|x| self.visit(x).map(Gc::new))
            .collect();

        ListOperations::built_in_list_func()(&items?).map(|x| (*x).clone())
    }

    fn visit_syntax_rules(&self, l: super::ast::SyntaxRules) -> Self::Output {
        todo!()
    }
}

struct TryFromSteelValForExprKind {}
