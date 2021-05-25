use crate::parser::ast::ExprKind;
use crate::parser::parser::SyntaxObject;
use crate::parser::tokens::TokenType;
use crate::parser::visitors::VisitorMutRef;

use std::collections::HashSet;

pub struct RenameIdentifiersVisitor<'a> {
    introduced_identifiers: HashSet<String>,
    pattern_variables: &'a [&'a str],
}

impl<'a> RenameIdentifiersVisitor<'a> {
    pub fn new(pattern_variables: &'a [&'a str]) -> Self {
        RenameIdentifiersVisitor {
            introduced_identifiers: HashSet::new(),
            pattern_variables,
        }
    }

    pub fn add(&mut self, ident: &str) {
        self.introduced_identifiers.insert(ident.to_string());
    }

    pub fn is_gensym(&self, ident: &str) -> bool {
        self.introduced_identifiers.contains(ident)
    }

    pub fn rename_identifiers(&mut self, expr: &mut ExprKind) {
        self.visit(expr);
    }
}

impl<'a> VisitorMutRef for RenameIdentifiersVisitor<'a> {
    type Output = ();

    fn visit_if(&mut self, f: &mut super::ast::If) -> Self::Output {
        self.visit(&mut f.test_expr);
        self.visit(&mut f.then_expr);
        self.visit(&mut f.else_expr);
    }

    fn visit_define(&mut self, define: &mut super::ast::Define) -> Self::Output {
        if let ExprKind::Atom(a) = &mut define.name {
            if let SyntaxObject {
                ty: TokenType::Identifier(ref s),
                ..
            } = a.syn
            {
                // If this is a special pattern variable, don't do any mangling of the variable
                if !self.pattern_variables.contains(&s.as_str()) {
                    self.add(s);
                    a.syn = SyntaxObject::default(TokenType::Identifier("##".to_string() + s));
                }
            }
        }

        self.visit(&mut define.body)
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
                    if !self.pattern_variables.contains(&s.as_str()) {
                        self.add(s);
                        a.syn = SyntaxObject::default(TokenType::Identifier("##".to_string() + s));
                    }
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

    fn visit_struct(&mut self, _s: &mut super::ast::Struct) -> Self::Output {
        // no op
    }

    fn visit_macro(&mut self, _m: &mut super::ast::Macro) -> Self::Output {
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

    fn visit_syntax_rules(&mut self, _l: &mut super::ast::SyntaxRules) -> Self::Output {
        todo!()
    }

    fn visit_set(&mut self, s: &mut super::ast::Set) -> Self::Output {
        self.visit(&mut s.variable);
        self.visit(&mut s.expr);
    }

    fn visit_require(&mut self, _s: &mut super::ast::Require) -> Self::Output {
        todo!()
    }

    fn visit_callcc(&mut self, cc: &mut super::ast::CallCC) -> Self::Output {
        self.visit(&mut cc.expr);
    }
}

#[cfg(test)]
mod rename_visitor_tests {

    use super::TokenType::*;
    use super::*;
    use crate::parser::ast::{Atom, Define, If, LambdaFunction, List, Transduce};

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.to_string(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
            n,
        ))))
    }

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

        let mut visitor = RenameIdentifiersVisitor::new(&[]);

        visitor.visit(&mut pre_condition);

        assert_eq!(pre_condition, post_condition);
    }

    #[test]
    fn test_should_do_nothing() {
        let mut pre_condition = ExprKind::If(Box::new(If::new(
            ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(true)))),
            ExprKind::Transduce(Box::new(Transduce::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "a".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "b".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "c".to_string(),
                )))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier(
                    "d".to_string(),
                )))),
                SyntaxObject::default(TokenType::Transduce),
            ))),
            ExprKind::If(Box::new(If::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(false)))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(10)))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(IntegerLiteral(20)))),
                SyntaxObject::default(If),
            ))),
            SyntaxObject::default(If),
        )));

        let post_condition = pre_condition.clone();

        let mut visitor = RenameIdentifiersVisitor::new(&[]);
        visitor.visit(&mut pre_condition);
        assert_eq!(pre_condition, post_condition);
    }

    #[test]
    fn test_rename_define() {
        let mut pre_condition: ExprKind = Define::new(
            atom_identifier("applesauce"),
            atom_int(10),
            SyntaxObject::default(TokenType::Define),
        )
        .into();

        let post_condition: ExprKind = Define::new(
            atom_identifier("##applesauce"),
            atom_int(10),
            SyntaxObject::default(TokenType::Define),
        )
        .into();

        let mut visitor = RenameIdentifiersVisitor::new(&[]);
        visitor.visit(&mut pre_condition);
        assert_eq!(pre_condition, post_condition);
    }

    #[test]
    fn test_rename_define_datum_syntax() {
        let mut pre_condition: ExprKind = Define::new(
            List::new(vec![
                atom_identifier("datum->syntax"),
                atom_identifier("escaping-definition"),
            ])
            .into(),
            atom_int(1),
            SyntaxObject::default(TokenType::Define),
        )
        .into();

        let post_condition: ExprKind = Define::new(
            List::new(vec![
                atom_identifier("datum->syntax"),
                atom_identifier("escaping-definition"),
            ])
            .into(),
            atom_int(1),
            SyntaxObject::default(TokenType::Define),
        )
        .into();

        let mut visitor = RenameIdentifiersVisitor::new(&[]);
        visitor.visit(&mut pre_condition);
        assert_eq!(pre_condition, post_condition);
    }
}
