use std::collections::HashSet;

use crate::{compiler::passes::VisitorMutUnit, parser::visitors::VisitorMut};
use crate::{parser::ast::ExprKind, rvals::Result};

/*
(define applesauce
    (bind/c
        (make-function/c
            (make/c int? 'int?)
            (make/c int? 'int?)
            (make/c int? 'int?)
            (make/c int? 'int?))
        (lambda (x y z) (begin (+ x y z)))
    applesauce))
*/

// Is this expression referring to a contract (is this a bind/c instance)
fn is_contract(expr: &ExprKind) -> bool {
    fn is_contract_option(expr: &ExprKind) -> Option<bool> {
        expr.list()?.first_ident().map(|x| x == "bind/c")
    }

    if let Some(inner) = is_contract_option(expr) {
        inner
    } else {
        false
    }
}

// Is this bind/c instance referring to a make-function/c instance
fn function_contract(expr: &ExprKind) -> Option<&ExprKind> {
    let body = expr.list()?;

    if body.first_ident()? == "bind/c" {
        let make_function = body.get(1)?;

        if make_function.list()?.first_ident()? == "make-function/c" {
            return Some(make_function);
        }
    }

    None
}

pub struct ContractCollector {
    contracts: HashSet<String>,
}

impl VisitorMutUnit for ContractCollector {
    fn visit_define(&mut self, define: &crate::parser::ast::Define) {
        todo!()
    }
}

pub struct ContractChecker {}

impl VisitorMut for ContractChecker {
    type Output = Result<()>;

    fn visit_if(&mut self, f: &crate::parser::ast::If) -> Self::Output {
        todo!()
    }

    fn visit_define(&mut self, define: &crate::parser::ast::Define) -> Self::Output {
        todo!()
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &crate::parser::ast::LambdaFunction,
    ) -> Self::Output {
        todo!()
    }

    fn visit_begin(&mut self, begin: &crate::parser::ast::Begin) -> Self::Output {
        todo!()
    }

    fn visit_return(&mut self, r: &crate::parser::ast::Return) -> Self::Output {
        todo!()
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
        todo!()
    }

    fn visit_set(&mut self, s: &crate::parser::ast::Set) -> Self::Output {
        todo!()
    }

    fn visit_require(&mut self, s: &crate::parser::ast::Require) -> Self::Output {
        todo!()
    }

    fn visit_callcc(&mut self, cc: &crate::parser::ast::CallCC) -> Self::Output {
        todo!()
    }

    fn visit_let(&mut self, l: &crate::parser::ast::Let) -> Self::Output {
        todo!()
    }
}
