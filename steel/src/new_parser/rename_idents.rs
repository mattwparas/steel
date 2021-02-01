use crate::new_parser::ast::Atom;
use crate::new_parser::ast::ExprKind;
use crate::new_parser::parser::SyntaxObject;
use crate::new_parser::tokens::TokenType;
use crate::new_parser::visitors::VisitorMutRef;

use std::collections::HashSet;

pub struct RenameIdentifiersVisitor {
    introduced_identifiers: HashSet<String>,
}

impl RenameIdentifiersVisitor {
    pub fn new() -> Self {
        RenameIdentifiersVisitor {
            introduced_identifiers: HashSet::new(),
        }
    }

    pub fn add(&mut self, ident: &str) {
        self.introduced_identifiers.insert(ident.to_string());
    }

    pub fn is_gensym(&self, ident: &str) -> bool {
        self.introduced_identifiers.contains(ident)
    }
}

impl VisitorMutRef for RenameIdentifiersVisitor {
    type Output = ();

    fn visit_if(&mut self, f: &mut super::ast::If) -> Self::Output {
        self.visit(&mut f.test_expr);
        self.visit(&mut f.then_expr);
        self.visit(&mut f.else_expr);
    }

    fn visit_define(&mut self, define: &mut super::ast::Define) -> Self::Output {
        todo!()
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        // todo!()
        for arg in &mut lambda_function.args {
            if let ExprKind::Atom(a) = arg {
                if let SyntaxObject {
                    ty: TokenType::Identifier(ref s),
                    ..
                } = a.syn
                {
                    self.add(s);
                    a.syn = SyntaxObject::default(TokenType::Identifier("##".to_string() + s));
                }
            }
        }

        self.visit(&mut lambda_function.body)
    }

    fn visit_begin(&mut self, begin: &mut super::ast::Begin) -> Self::Output {
        for expr in &mut begin.exprs {
            self.visit(expr)
        }
    }

    fn visit_return(&mut self, r: &mut super::ast::Return) -> Self::Output {
        self.visit(&mut r.expr)
    }

    fn visit_apply(&mut self, apply: &mut super::ast::Apply) -> Self::Output {
        self.visit(&mut apply.func);
        self.visit(&mut apply.list);
    }

    fn visit_panic(&mut self, p: &mut super::ast::Panic) -> Self::Output {
        self.visit(&mut p.message);
    }

    fn visit_transduce(&mut self, transduce: &mut super::ast::Transduce) -> Self::Output {
        // todo!()
        self.visit(&mut transduce.transducer);
        self.visit(&mut transduce.func);
        self.visit(&mut transduce.initial_value);
        self.visit(&mut transduce.iterable);
    }

    fn visit_read(&mut self, read: &mut super::ast::Read) -> Self::Output {
        self.visit(&mut read.expr);
    }

    fn visit_execute(&mut self, execute: &mut super::ast::Execute) -> Self::Output {
        self.visit(&mut execute.transducer);
        self.visit(&mut execute.collection);

        if let Some(ref mut o) = execute.output_type {
            self.visit(o);
        }
    }

    fn visit_quote(&mut self, quote: &mut super::ast::Quote) -> Self::Output {
        self.visit(&mut quote.expr);
    }

    fn visit_struct(&mut self, s: &mut super::ast::Struct) -> Self::Output {
        todo!()
    }

    fn visit_macro(&mut self, m: &mut super::ast::Macro) -> Self::Output {
        todo!()
    }

    fn visit_eval(&mut self, e: &mut super::ast::Eval) -> Self::Output {
        self.visit(&mut e.expr);
    }

    fn visit_atom(&mut self, a: &mut super::ast::Atom) -> Self::Output {
        let token = a.syn.ty.clone();
        if let TokenType::Identifier(s) = token {
            if self.is_gensym(&s) {
                a.syn.ty = TokenType::Identifier("##".to_string() + &s);
            }
        }
    }

    fn visit_list(&mut self, l: &mut super::ast::List) -> Self::Output {
        for expr in &mut l.args {
            self.visit(expr);
        }
    }

    fn visit_syntax_rules(&mut self, l: &mut super::ast::SyntaxRules) -> Self::Output {
        todo!()
    }
}

#[cfg(test)]
mod rename_visitor_tests {

    use super::TokenType::*;
    use super::*;
    use crate::new_parser::ast::{LambdaFunction, List};

    #[test]
    fn test_rename_identifiers() {
        let mut pre_condition = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
            vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier("x".to_string()),
            )))],
            ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "+".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "x".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
            ])),
            SyntaxObject::default(TokenType::Lambda),
        )));

        let post_condition = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
            vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier("##x".to_string()),
            )))],
            ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "+".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "##x".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
            ])),
            SyntaxObject::default(TokenType::Lambda),
        )));

        let mut visitor = RenameIdentifiersVisitor::new();

        visitor.visit(&mut pre_condition);

        assert_eq!(pre_condition, post_condition);
    }
}
