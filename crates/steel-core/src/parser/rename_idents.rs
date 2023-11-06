use crate::parser::ast::ExprKind;
use crate::parser::parser::SyntaxObject;
use crate::parser::tokens::TokenType;
use crate::parser::visitors::VisitorMutRef;

use std::collections::HashSet;

use super::interner::InternedString;

pub struct RenameIdentifiersVisitor<'a> {
    introduced_identifiers: HashSet<InternedString>,
    pattern_variables: &'a [&'a InternedString],
    syntax: &'a [InternedString],
}

impl<'a> RenameIdentifiersVisitor<'a> {
    pub fn new(pattern_variables: &'a [&'a InternedString], syntax: &'a [InternedString]) -> Self {
        RenameIdentifiersVisitor {
            introduced_identifiers: HashSet::new(),
            pattern_variables,
            syntax,
        }
    }

    pub fn add(&mut self, ident: InternedString) {
        self.introduced_identifiers.insert(ident);
    }

    pub fn is_gensym(&self, ident: &InternedString) -> bool {
        self.introduced_identifiers.contains(ident) || self.pattern_variables.contains(&ident)
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
                if !self.pattern_variables.contains(&s) {
                    self.add(*s);
                    // a.syn = SyntaxObject::default(TokenType::Identifier("##".to_string() + s));
                }

                a.syn = SyntaxObject::default(TokenType::Identifier(
                    ("##".to_string() + s.resolve()).into(),
                ));
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
                    if !self.pattern_variables.contains(&s) {
                        self.add(*s);
                        // a.syn = SyntaxObject::default(TokenType::Identifier("##".to_string() + s));
                    }

                    a.syn = SyntaxObject::default(TokenType::Identifier(
                        ("##".to_string() + s.resolve()).into(),
                    ));
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

    fn visit_quote(&mut self, quote: &mut super::ast::Quote) -> Self::Output {
        self.visit(&mut quote.expr);
    }

    fn visit_macro(&mut self, _m: &mut super::ast::Macro) -> Self::Output {
        todo!()
    }

    fn visit_atom(&mut self, a: &mut super::ast::Atom) -> Self::Output {
        let token = a.syn.ty.clone();
        if let TokenType::Identifier(s) = token {
            if self.syntax.contains(&s) {
                return;
            }

            if self.is_gensym(&s) {
                a.syn.ty = TokenType::Identifier(("##".to_string() + s.resolve()).into());
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

    fn visit_require(&mut self, s: &mut super::ast::Require) -> Self::Output {
        for module in &mut s.modules {
            self.visit(module);
        }
    }

    fn visit_let(&mut self, l: &mut super::ast::Let) -> Self::Output {
        for (_, expr) in &mut l.bindings {
            self.visit(expr);
        }

        self.visit(&mut l.body_expr);
    }
}

#[cfg(test)]
mod rename_visitor_tests {

    use steel_parser::tokens::MaybeBigInt;

    use super::TokenType::*;
    use super::*;
    use crate::parser::ast::{Atom, Define, If, LambdaFunction, List};

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::IntegerLiteral(
            MaybeBigInt::Small(n),
        ))))
    }

    #[test]
    fn test_rename_identifiers() {
        let mut pre_condition = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
            vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier("x".into()),
            )))],
            ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("+".into())))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("x".into())))),
                atom_int(10),
            ])),
            SyntaxObject::default(TokenType::Lambda),
        )));

        let post_condition = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
            vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier("##x".into()),
            )))],
            ExprKind::List(List::new(vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("+".into())))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("##x".into())))),
                atom_int(10),
            ])),
            SyntaxObject::default(TokenType::Lambda),
        )));

        let mut visitor = RenameIdentifiersVisitor::new(&[], &[]);

        visitor.visit(&mut pre_condition);

        assert_eq!(pre_condition, post_condition);
    }

    #[test]
    fn test_should_do_nothing() {
        let mut pre_condition = ExprKind::If(Box::new(If::new(
            ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(true)))),
            ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(true)))),
            ExprKind::If(Box::new(If::new(
                ExprKind::Atom(Atom::new(SyntaxObject::default(BooleanLiteral(false)))),
                atom_int(10),
                atom_int(20),
                SyntaxObject::default(If),
            ))),
            SyntaxObject::default(If),
        )));

        let post_condition = pre_condition.clone();

        let mut visitor = RenameIdentifiersVisitor::new(&[], &[]);
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

        let mut visitor = RenameIdentifiersVisitor::new(&[], &[]);
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

        let mut visitor = RenameIdentifiersVisitor::new(&[], &[]);
        visitor.visit(&mut pre_condition);
        assert_eq!(pre_condition, post_condition);
    }
}
