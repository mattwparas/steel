use steel_parser::ast::{Atom, LAMBDA, LAMBDA_FN, PLAIN_LET};

use crate::compiler::program::{DATUM_SYNTAX, LAMBDA_SYMBOL};
use crate::parser::ast::ExprKind;
use crate::parser::parser::SyntaxObject;
use crate::parser::tokens::TokenType;
use crate::parser::visitors::VisitorMutRef;

use std::collections::HashSet;

use super::interner::InternedString;

pub struct RenameIdentifiersVisitor<'a> {
    introduced_identifiers: HashSet<InternedString>,
    pattern_variables: &'a [InternedString],
    syntax: &'a [InternedString],
}

impl<'a> RenameIdentifiersVisitor<'a> {
    pub fn new(pattern_variables: &'a [InternedString], syntax: &'a [InternedString]) -> Self {
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
        self.introduced_identifiers.contains(ident) || self.pattern_variables.contains(ident)
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
                if !self.pattern_variables.contains(s) {
                    self.add(*s);
                    // a.syn = SyntaxObject::default(TokenType::Identifier("##".to_string() + s));
                }

                a.syn = SyntaxObject::default(TokenType::Identifier(
                    ("##".to_string() + s.resolve()).into(),
                ));
                a.syn.introduced_via_macro = true;
            }
        }

        self.visit(&mut define.body)
    }

    fn visit_lambda_function(
        &mut self,
        lambda_function: &mut super::ast::LambdaFunction,
    ) -> Self::Output {
        for arg in &mut lambda_function.args {
            if let ExprKind::Atom(a) = arg {
                if let SyntaxObject {
                    ty: TokenType::Identifier(ref s),
                    ..
                } = a.syn
                {
                    if !self.pattern_variables.contains(s) {
                        self.add(*s);
                    }

                    a.syn = SyntaxObject::default(TokenType::Identifier(
                        ("##".to_string() + s.resolve()).into(),
                    ));
                    a.syn.introduced_via_macro = true;
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
            if self.syntax.contains(&s) || s == *DATUM_SYNTAX {
                return;
            }

            if self.is_gensym(&s) {
                a.syn.ty = TokenType::Identifier(("##".to_string() + s.resolve()).into());
            } else {
                a.syn.unresolved = true;
            }
        }
    }

    fn visit_list(&mut self, l: &mut super::ast::List) -> Self::Output {
        // TODO: So here, since this body isn't lowered all the way, we're not
        // actually _doing_ anything!
        match l.first() {
            // Some(ExprKind::Atom(Atom {
            //     syn: SyntaxObject { ty, .. },
            // })) if *ty == TokenType::Identifier(*DATUM_SYNTAX) => {
            //     for expr in &mut l.args {
            //         self.visit(expr);
            //     }
            // }
            Some(ExprKind::Atom(Atom {
                syn: SyntaxObject { ty, .. },
            })) if *ty == TokenType::Define => {
                // TODO: Top level defines should be allowed to escape here
                match l.args.get_mut(1) {
                    // (define name expr)
                    Some(ExprKind::Atom(a)) => {
                        if let SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        } = a.syn
                        {
                            if !self.pattern_variables.contains(&s) {
                                self.add(s);
                            }

                            a.syn = SyntaxObject::default(TokenType::Identifier(
                                ("##".to_string() + s.resolve()).into(),
                            ));
                            a.syn.introduced_via_macro = true;

                            // TODO: Move the logic about resolving
                            // the name actually back to the expand visitor.
                            // This doesn't _quite_ make sense - this is the template
                            // value that is introduced. For example, in the example:
                            //
                            // (define-syntax letrec*-helper
                            //   (syntax-rules ()
                            //     [(letrec*-helper () body ...)
                            //      (begin
                            //        body ...)]
                            //     [(letrec*-helper ((var val) rest ...) body ...)
                            //      (begin
                            //        (define var val)
                            //        (letrec*-helper (rest ...) body ...))]))
                            // a.syn.previous_name = Some(s);
                        }
                    }

                    // (define (name args ...) exprs ...)
                    Some(ExprKind::List(shorthand_define_function)) => {
                        for arg in shorthand_define_function.args.iter_mut() {
                            if let ExprKind::Atom(a) = arg {
                                if let SyntaxObject {
                                    ty: TokenType::Identifier(s),
                                    ..
                                } = a.syn
                                {
                                    if s != *DATUM_SYNTAX {
                                        if !self.pattern_variables.contains(&s) {
                                            self.add(s);
                                        }

                                        a.syn = SyntaxObject::default(TokenType::Identifier(
                                            ("##".to_string() + s.resolve()).into(),
                                        ));
                                        a.syn.introduced_via_macro = true;
                                        // a.syn.previous_name = Some(s);
                                    }
                                }
                            }
                        }
                    }

                    _ => {}
                }

                if let Some(rest) = l.args.get_mut(2..) {
                    for expr in rest.iter_mut() {
                        self.visit(expr)
                    }
                }
            }

            Some(ExprKind::Atom(Atom {
                syn: SyntaxObject { ty, .. },
            })) if *ty == TokenType::Lambda
                || *ty == TokenType::Identifier(*LAMBDA_SYMBOL)
                || *ty == TokenType::Identifier(*LAMBDA)
                || *ty == TokenType::Identifier(*LAMBDA_FN) =>
            {
                let arguments = l.args.get_mut(1);

                if let Some(ExprKind::List(arguments)) = arguments {
                    for arg in arguments.args.iter_mut() {
                        if let ExprKind::Atom(a) = arg {
                            if let SyntaxObject {
                                ty: TokenType::Identifier(s),
                                ..
                            } = a.syn
                            {
                                if !self.pattern_variables.contains(&s) {
                                    self.add(s);
                                }

                                a.syn = SyntaxObject::default(TokenType::Identifier(
                                    ("##".to_string() + s.resolve()).into(),
                                ));
                                a.syn.introduced_via_macro = true;
                                // a.syn.previous_name = Some(s);
                            }
                        }
                    }
                }

                if let Some(ExprKind::Atom(a)) = arguments {
                    if let SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    } = a.syn
                    {
                        if !self.pattern_variables.contains(&s) {
                            self.add(s);
                        }

                        a.syn = SyntaxObject::default(TokenType::Identifier(
                            ("##".to_string() + s.resolve()).into(),
                        ));
                        a.syn.introduced_via_macro = true;
                        // a.syn.previous_name = Some(s);
                    }
                }

                if let Some(rest) = l.args.get_mut(2..) {
                    for expr in rest.iter_mut() {
                        self.visit(expr)
                    }
                }
            }

            Some(ExprKind::Atom(Atom {
                syn: SyntaxObject { ty, .. },
            })) if *ty == TokenType::Let || *ty == TokenType::Identifier(*PLAIN_LET) => {
                match l.args.get_mut(1) {
                    Some(ExprKind::List(bindings)) => {
                        for pair in &mut bindings.args {
                            if let ExprKind::List(p) = pair {
                                if let Some(ExprKind::Atom(a)) = p.args.get_mut(0) {
                                    if let SyntaxObject {
                                        ty: TokenType::Identifier(s),
                                        ..
                                    } = a.syn
                                    {
                                        if !self.pattern_variables.contains(&s) {
                                            self.add(s);
                                        }

                                        a.syn = SyntaxObject::default(TokenType::Identifier(
                                            ("##".to_string() + s.resolve()).into(),
                                        ));
                                        a.syn.introduced_via_macro = true;
                                        // a.syn.previous_name = Some(s);
                                    }
                                }

                                if let Some(expr) = p.args.get_mut(1) {
                                    self.visit(expr)
                                }
                            }
                        }
                    }

                    // `let loop ((binding expr) ...)`
                    Some(ExprKind::Atom(a)) => {
                        if let SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        } = a.syn
                        {
                            if !self.pattern_variables.contains(&s) {
                                self.add(s);
                            }

                            a.syn = SyntaxObject::default(TokenType::Identifier(
                                ("##".to_string() + s.resolve()).into(),
                            ));
                            a.syn.introduced_via_macro = true;
                            // a.syn.previous_name = Some(s);
                        }

                        // Catch the bindings as well
                        if let Some(ExprKind::List(bindings)) = l.args.get_mut(2) {
                            for pair in &mut bindings.args {
                                if let ExprKind::List(p) = pair {
                                    if let Some(ExprKind::Atom(a)) = p.args.get_mut(0) {
                                        if let SyntaxObject {
                                            ty: TokenType::Identifier(s),
                                            ..
                                        } = a.syn
                                        {
                                            if !self.pattern_variables.contains(&s) {
                                                self.add(s);
                                            }

                                            a.syn = SyntaxObject::default(TokenType::Identifier(
                                                ("##".to_string() + s.resolve()).into(),
                                            ));
                                            a.syn.introduced_via_macro = true;
                                            // a.syn.previous_name = Some(s);
                                        }
                                    }

                                    if let Some(expr) = p.args.get_mut(1) {
                                        self.visit(expr)
                                    }
                                }
                            }
                        }
                    }

                    _ => {}
                }

                if let Some(remaining) = l.args.get_mut(2..) {
                    for expr in remaining {
                        self.visit(expr);
                    }
                }
            }

            _ => {
                for expr in &mut l.args {
                    self.visit(expr);
                }
            }
        }

        // for expr in &mut l.args {
        //     self.visit(expr);
        // }
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

    // TODO: This needs to be fixed!
    fn visit_let(&mut self, l: &mut super::ast::Let) -> Self::Output {
        for (arg, expr) in &mut l.bindings {
            if let ExprKind::Atom(a) = arg {
                if let SyntaxObject {
                    ty: TokenType::Identifier(ref s),
                    ..
                } = a.syn
                {
                    if !self.pattern_variables.contains(s) {
                        self.add(*s);
                    }

                    a.syn = SyntaxObject::default(TokenType::Identifier(
                        ("##".to_string() + s.resolve()).into(),
                    ));
                    a.syn.introduced_via_macro = true;
                }
            }

            self.visit(expr);
        }

        self.visit(&mut l.body_expr);
    }

    fn visit_vector(&mut self, v: &mut super::ast::Vector) -> Self::Output {
        if v.bytes {
            return;
        }

        for arg in &mut v.args {
            self.visit(arg);
        }
    }
}

#[cfg(test)]
mod rename_visitor_tests {

    use steel_parser::tokens::IntLiteral;

    use super::TokenType::*;
    use super::*;
    use crate::parser::ast::{Atom, Define, If, LambdaFunction, List};

    use thin_vec::thin_vec;

    fn atom_identifier(s: &str) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
            s.into(),
        ))))
    }

    fn atom_int(n: isize) -> ExprKind {
        ExprKind::Atom(Atom::new(SyntaxObject::default(
            IntLiteral::Small(n).into(),
        )))
    }

    #[test]
    fn test_rename_identifiers() {
        let mut pre_condition = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
            thin_vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier("x".into()),
            )))],
            ExprKind::List(List::new(thin_vec![
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("+".into())))),
                ExprKind::Atom(Atom::new(SyntaxObject::default(Identifier("x".into())))),
                atom_int(10),
            ])),
            SyntaxObject::default(TokenType::Lambda),
        )));

        let post_condition = ExprKind::LambdaFunction(Box::new(LambdaFunction::new(
            thin_vec![ExprKind::Atom(Atom::new(SyntaxObject::default(
                Identifier("##x".into()),
            )))],
            ExprKind::List(List::new(thin_vec![
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
            List::new(thin_vec![
                atom_identifier("datum->syntax"),
                atom_identifier("escaping-definition"),
            ])
            .into(),
            atom_int(1),
            SyntaxObject::default(TokenType::Define),
        )
        .into();

        let post_condition: ExprKind = Define::new(
            List::new(thin_vec![
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
