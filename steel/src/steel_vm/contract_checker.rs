use std::collections::HashMap;

use crate::{
    compiler::passes::{VisitorMutUnit, VisitorMutUnitRef},
    parser::visitors::VisitorMut,
};
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

#[derive(Debug)]
pub struct BindContract<'a> {
    make_function: &'a ExprKind,
    body: &'a ExprKind,
}

impl<'a> BindContract<'a> {
    pub fn new(make_function: &'a ExprKind, body: &'a ExprKind) -> Self {
        BindContract {
            make_function,
            body,
        }
    }
}

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
fn function_contract<'a>(expr: &'a ExprKind) -> Option<BindContract<'a>> {
    let body = expr.list()?;

    if body.first_ident()? == "bind/c" {
        let make_function = body.get(1)?;

        if make_function.list()?.first_ident()? == "make-function/c" {
            // This just deconstructs the (bind/c into a individual struct for access)
            return Some(BindContract::new(make_function, body.get(2)?));
        }
    }

    None
}

#[derive(Default, Debug)]
pub struct GlobalContractCollector<'a> {
    contracts: HashMap<String, BindContract<'a>>,
}

impl<'a> GlobalContractCollector<'a> {
    pub fn collect_contracts(exprs: &'a [ExprKind]) -> Self {
        let mut collector = Self::default();

        exprs.iter().for_each(|x| collector.visit(x));

        collector
    }
}

impl<'a> VisitorMutUnitRef<'a> for GlobalContractCollector<'a> {
    fn visit_define(&mut self, define: &'a crate::parser::ast::Define) {
        if let Some(contract) = function_contract(&define.body) {
            // If we try to collect contracts on something thats not expanded, we'll have problems anyway
            self.contracts
                .insert(define.name.atom_identifier().unwrap().to_string(), contract);
        }
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
