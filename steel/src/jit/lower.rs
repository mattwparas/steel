use crate::parser::ast;
use crate::parser::ast::ExprKind;
use crate::parser::visitors::VisitorMut;

use std::collections::HashSet;

use super::ir::Expr;

// ((lambda (x y z) (+ x y z)) 10 20 30)
// =>
// (x = 10)
// (y = 20)
// (z = 30)
// (+ x (+ y z))
struct RenameShadowedVars {
    vars: HashSet<String>,
}

impl VisitorMut for RenameShadowedVars {
    type Output = Option<Expr>;

    // Not great, but gets the job done
    fn visit_if(&mut self, f: &ast::If) -> Self::Output {
        let test = Box::new(self.visit(&f.test_expr)?);
        let then_expr = self.visit(&f.then_expr)?;
        let else_expr = self.visit(&f.else_expr)?;

        match (&then_expr, &else_expr) {
            (Expr::Block(t), Expr::Block(e)) => Some(Expr::IfElse(test, t.clone(), e.clone())),

            (Expr::Block(t), _) => Some(Expr::IfElse(test, t.clone(), vec![else_expr])),

            (_, Expr::Block(e)) => Some(Expr::IfElse(test, vec![then_expr], e.clone())),

            (_, _) => Some(Expr::IfElse(test, vec![then_expr], vec![else_expr])),
        }
    }

    // Define -> Assignment
    fn visit_define(&mut self, define: &ast::Define) -> Self::Output {
        let body = self.visit(&define.body)?;
        Some(Expr::Assign(
            define
                .name
                .atom_identifier_or_else(|| unreachable!())
                .ok()?
                .to_owned(),
            Box::new(body),
        ))
    }

    fn visit_lambda_function(&mut self, lambda_function: &ast::LambdaFunction) -> Self::Output {
        todo!()
    }

    fn visit_begin(&mut self, begin: &ast::Begin) -> Self::Output {
        todo!()
    }

    fn visit_return(&mut self, r: &ast::Return) -> Self::Output {
        None
    }

    fn visit_apply(&mut self, apply: &ast::Apply) -> Self::Output {
        None
    }

    fn visit_panic(&mut self, p: &ast::Panic) -> Self::Output {
        None
    }

    fn visit_transduce(&mut self, transduce: &ast::Transduce) -> Self::Output {
        None
    }

    fn visit_read(&mut self, read: &ast::Read) -> Self::Output {
        None
    }

    fn visit_execute(&mut self, execute: &ast::Execute) -> Self::Output {
        None
    }

    fn visit_quote(&mut self, quote: &ast::Quote) -> Self::Output {
        None
    }

    fn visit_struct(&mut self, s: &ast::Struct) -> Self::Output {
        None
    }

    fn visit_macro(&mut self, m: &ast::Macro) -> Self::Output {
        None
    }

    fn visit_eval(&mut self, e: &ast::Eval) -> Self::Output {
        None
    }

    fn visit_atom(&mut self, a: &ast::Atom) -> Self::Output {
        todo!()
    }

    // Check explicit function application with let
    fn visit_list(&mut self, l: &ast::List) -> Self::Output {
        todo!()
    }

    fn visit_syntax_rules(&mut self, l: &ast::SyntaxRules) -> Self::Output {
        todo!()
    }

    // This should be assignment
    // but only if the variable being assigned is scoped locally to the function
    fn visit_set(&mut self, s: &ast::Set) -> Self::Output {
        todo!()
    }

    fn visit_require(&mut self, s: &ast::Require) -> Self::Output {
        None
    }

    fn visit_callcc(&mut self, cc: &ast::CallCC) -> Self::Output {
        None
    }
}
